/* author Kevin Smith <ksmith@basho.com>
   copyright 2009-2010 Basho Technologies

   SPDX-FileCopyrightText: 2009-2010 Basho Technologies
   SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
   SPDX-License-Identifier: Apache-2.0 */

#include <cstring>
#include <ctime>
#include <unistd.h>

#include <js/CompilationAndEvaluation.h>
#include <js/ContextOptions.h>
#include <js/Conversions.h>
#include <js/Initialization.h>
#include <js/SourceText.h>
#include <js/Warnings.h>
#include <jsapi.h>

#include "spidermonkey.h"

/* The class of the global object. */
static constexpr JSClass global_class = {"global", JSCLASS_GLOBAL_FLAGS,
                                         &JS::DefaultGlobalClassOps};

void on_error(JSContext* context, JSErrorReport* report)
{
    if (!report->isWarning())
    {
        auto* state =
            static_cast<spidermonkey_state*>(JS_GetContextPrivate(context));
        /* report->linebuf() returns const char16_t* in mozjs 115+;
           casting to (char*) is undefined behaviour.  Use a safe
           placeholder instead. */
        state->replace_error(report->message().c_str(), report->lineno,
                             "<source unavailable>");
        JS_SetContextPrivate(context, state);
    }
}

bool on_branch(JSContext* context)
{
    bool return_value = true;
    auto* state = static_cast<spidermonkey_state*>(JS_GetContextPrivate(context));
    state->branch_count++;

    if (state->terminate)
    {
        JS_GC(context);
        return_value = false;
    }
    else if (state->branch_count == 550)
    {
        JS_GC(context);
        state->branch_count = 0;
    }
    else if (state->branch_count % 100 == 0)
    {
        JS_MaybeGC(context);
    }

    JS_SetContextPrivate(context, state);
    return return_value;
}

bool js_log(JSContext* cx, unsigned argc, JS::Value* vp)
{
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);

    if (argc != 2)
    {
        args.rval().setBoolean(false);
    }
    else
    {
        JS::RootedString file_str(cx, JS::ToString(cx, args[0]));
        if (!file_str)
        {
            args.rval().setBoolean(false);
            return true;
        }
        JS::UniqueChars filename(JS_EncodeStringToUTF8(cx, file_str));
        if (!filename)
        {
            args.rval().setBoolean(false);
            return true;
        }

        JS::RootedString out_str(cx, JS::ToString(cx, args[1]));
        if (!out_str)
        {
            args.rval().setBoolean(false);
            return true;
        }
        JS::UniqueChars output(JS_EncodeStringToUTF8(cx, out_str));
        if (!output)
        {
            args.rval().setBoolean(false);
            return true;
        }

        FILE* fd = fopen(filename.get(), "a+");
        if (fd)
        {
            struct tm tmp;
            time_t t;

            t = time(nullptr);
            if (localtime_r(&t, &tmp) != nullptr)
            {
                fprintf(fd, "%02d/%02d/%04d (%02d:%02d:%02d): ", tmp.tm_mon + 1, tmp.tm_mday,
                        tmp.tm_year + 1900, tmp.tm_hour, tmp.tm_min, tmp.tm_sec);

                fwrite(output.get(), 1, strlen(output.get()), fd);
                fwrite("\n", 1, 1, fd);
                fclose(fd);

                args.rval().setBoolean(true);
            }
            else
            {
                fclose(fd);
                args.rval().setBoolean(false);
            }
        }
        else
        {
            args.rval().setBoolean(false);
        }
    }
    return true;
}

void spidermonkey_state::replace_error(const char* m, unsigned int l, const char* os)
{
    free_error();

    msg = new std::string(m);
    lineno = l;
    if (os)
        offending_source = new std::string(os);
    else
        offending_source = new std::string("<internally_generated>");
    error = true;
}

char* spidermonkey_state::error_to_json()
{
    std::string* escaped_source = new std::string();
    bool escaped = false;
    for (char c : *offending_source)
    {
        if (c == '\\')
        {
            *escaped_source += c;
            escaped = true;
        }
        else
        {
            if ((c == '"') && !escaped)
                *escaped_source += "\\\"";
            else
                *escaped_source += c;
            escaped = false;
        }
    }

    const char fmt[] = "{\"lineno\": %u, \"message\": \"%s\", \"source\": \"%s\"}";
    size_t size = escaped_source->length() + msg->length() + strlen(fmt);
    char* retval = new char[size];

    snprintf(retval, size, fmt, lineno, msg->c_str(), escaped_source->c_str());
    delete escaped_source;

    free_error();
    return retval;
}

spidermonkey_vm::spidermonkey_vm(size_t thread_stack, uint32_t heap_size)
    : context(nullptr), global()
{

/* Bytes to allocate before GC */
#define MAX_GC_SIZE 1024 * 1024

    context = JS_NewContext(MAX_GC_SIZE);

    JS::InitSelfHostedCode(context);

    JS_SetNativeStackQuota(context, thread_stack);
    JS_SetGCParameter(context, JSGC_MAX_BYTES, heap_size);

    JS::ContextOptionsRef(context).setAsmJS(true);

    JS_SetGlobalJitCompilerOption(context, JSJitCompilerOption::JSJITCOMPILER_ION_ENABLE, true);
    JS_SetGlobalJitCompilerOption(context, JSJitCompilerOption::JSJITCOMPILER_BASELINE_ENABLE,
                                  true);

    JS::RealmOptions options;

    spidermonkey_state* state = new spidermonkey_state();

    JS::RootedObject g(context, JS_NewGlobalObject(context, &global_class, nullptr,
                                                   JS::FireOnNewGlobalHook, options));
    this->global.init(context, g);

    JSAutoRealm ar(context, g);

    /*
     * JS_InitReflectParse and JS_DefineDebuggerObject were available in
     * older SpiderMonkey versions but have been removed. They are not
     * essential for erlang-mozjs's use case (eval/call only).
     */

    JS::SetWarningReporter(context, on_error);
    JS_AddInterruptCallback(context, on_branch);
    JS_SetContextPrivate(context, state);
    JS_DefineFunction(context, g, "ejsLog", js_log, 0, 0);
}

spidermonkey_vm::~spidermonkey_vm()
{
    auto* state = static_cast<spidermonkey_state*>(JS_GetContextPrivate(this->context));
    JS_SetContextPrivate(this->context, nullptr);

    delete state;

    this->global.reset();
    JS_DestroyContext(this->context);
}

void spidermonkey_vm::sm_stop()
{
    auto* state = static_cast<spidermonkey_state*>(JS_GetContextPrivate(this->context));
    state->terminate = true;
    JS_SetContextPrivate(this->context, state);

    /* Request interrupt callback immediately. This call is thread-safe. */
    JS_RequestInterruptCallback(this->context);
}

bool spidermonkey_vm::sm_eval(const char* filename, size_t filename_length, const char* code,
                              size_t code_length, char** output, int handle_retval)
{

    JSAutoRealm ar(this->context, this->global);

    char* filename0 = strndup(filename, filename_length);
    JS::CompileOptions options(this->context);
    options.setFileAndLine(filename0, 1);
    free(filename0);

    JS::SourceText<mozilla::Utf8Unit> source;
    if (!source.init(this->context, code, code_length, JS::SourceOwnership::Borrowed))
    {
        return false;
    }

    JS::RootedScript script(this->context, JS::Compile(this->context, options, source));
    if (!script)
    {
        this->check_js_exception();
        auto* state = static_cast<spidermonkey_state*>(JS_GetContextPrivate(this->context));
        if (!state->error)
        {
            /* check_js_exception didn't capture a structured error.
               In some SpiderMonkey versions, compile errors are reported
               via the WarningReporter callback without setting a pending
               exception.  Synthesize a fallback. */
            state->replace_error("compilation failed", 0, "<unknown>");
        }
        *output = state->error_to_json();
        return false;
    }

    auto* state = static_cast<spidermonkey_state*>(JS_GetContextPrivate(this->context));

    JS::RootedValue result(this->context);
    bool exec_ok = JS_ExecuteScript(this->context, script, &result);
    if (!exec_ok)
        this->check_js_exception();
    if (state->error)
    {
        *output = state->error_to_json();
        return false;
    }

    /* If execution failed but check_js_exception didn't capture a
       structured error, synthesize a fallback — don't fall through
       to the success path with an undefined result value.
       Exception: if the script was interrupted via sm_stop(), let it
       fall through so handle_retval produces "undefined" which the
       Erlang side maps to {error, mozjs_script_interrupted}. */
    if (!exec_ok && !state->terminate)
    {
        state->replace_error("script execution failed", 0, "<unknown>");
        *output = state->error_to_json();
        return false;
    }

    if (handle_retval)
    {
        JS::RootedString str(this->context, JS::ToString(this->context, result));
        JS::UniqueChars buf(JS_EncodeStringToUTF8(this->context, str));
        size_t size = strlen(buf.get());
        *output = new char[size + 1];
        strncpy(*output, buf.get(), size + 1);
    }

    return true;
}

void spidermonkey_vm::check_js_exception()
{
    if (JS_IsExceptionPending(this->context))
    {
        JS::RootedValue exception_v(this->context);
        JS_GetPendingException(this->context, &exception_v);
        JS_ClearPendingException(this->context);

        if (exception_v.isObject())
        {
            JS::RootedObject exception(this->context, &exception_v.toObject());
            JSErrorReport* report = JS_ErrorFromException(this->context, exception);
            if (report)
            {
                auto* state =
                    static_cast<spidermonkey_state*>(JS_GetContextPrivate(this->context));
                state->replace_error(report->message().c_str(), report->lineno,
                                     "<exception>");
                JS_SetContextPrivate(this->context, state);
                return;
            }
        }

        /* Exception is not a standard Error object (e.g. throw("string"))
           or JS_ErrorFromException returned null (user-created Error).
           Stringify the exception value — safe now that we cleared the
           pending exception above. */
        JS::RootedString str(this->context, JS::ToString(this->context, exception_v));
        if (str)
        {
            JS::UniqueChars buf(JS_EncodeStringToUTF8(this->context, str));
            if (buf)
            {
                auto* state =
                    static_cast<spidermonkey_state*>(JS_GetContextPrivate(this->context));
                state->replace_error(buf.get(), 0, "<thrown>");
                JS_SetContextPrivate(this->context, state);
            }
        }
    }
}
