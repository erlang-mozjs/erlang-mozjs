/* author Kevin Smith <ksmith@basho.com>
   copyright 2009-2010 Basho Technologies

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

#include <unistd.h>
#include <ctime>
#include <cstring>

#include <jsapi.h>
#include <js/Initialization.h>
#include <js/CompilationAndEvaluation.h>
#include <js/ContextOptions.h>
#include <js/Conversions.h>
#include <js/SourceText.h>
#include <js/Warnings.h>

#include "spidermonkey.h"

/* The class of the global object. */
static constexpr JSClass global_class = {
    "global", JSCLASS_GLOBAL_FLAGS, &JS::DefaultGlobalClassOps
};

void on_error(JSContext *context, JSErrorReport *report) {
  if (!report->isWarning()) {
    spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(context);
    // FIXME FIXME FIXME remove char* cast here from char16*
    state->replace_error(report->message().c_str(), report->lineno, (char*)(report->linebuf()));
    JS_SetContextPrivate(context, state);
  }
}

bool on_branch(JSContext *context) {
  bool return_value = true;
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(context);
  state->branch_count++;

  if (state->terminate)  {
      JS_GC(context);
      return_value = false;
  }
  else if (state->branch_count == 550) {
    JS_GC(context);
    state->branch_count = 0;
  }
  else if(state->branch_count % 100 == 0) {
    JS_MaybeGC(context);
  }

  JS_SetContextPrivate(context, state);
  return return_value;
}

bool js_log(JSContext *cx, unsigned argc, JS::Value *vp) {
  JS::CallArgs args = JS::CallArgsFromVp(argc, vp);

  if (argc != 2) {
    args.rval().setBoolean(false);
  }
  else {
    // FIXME
    auto file_str = JS::ToString(cx, args[0]);
    size_t file_size = JS_GetStringLength(file_str);
    char* filename = (char*)malloc(file_size + 1);
    file_size = JS_EncodeStringToBuffer(cx, file_str, filename, file_size+1);

    // FIXME
    auto out_str = JS::ToString(cx, args[1]);
    size_t out_size = JS_GetStringLength(out_str);
    char* output = (char*)malloc(out_size + 1);
    out_size = JS_EncodeStringToBuffer(cx, out_str, output, out_size+1);

    FILE *fd = fopen(filename, "a+");
    if (fd) {
      struct tm *tmp;
      time_t t;

      t = time(NULL);
      tmp = localtime(&t); /* or gmtime, if you want GMT^H^H^HUTC */
      fprintf(fd, "%02d/%02d/%04d (%02d:%02d:%02d): ",
              tmp->tm_mon+1, tmp->tm_mday, tmp->tm_year+1900,
              tmp->tm_hour, tmp->tm_min, tmp->tm_sec);

      fwrite(output, 1, strlen(output), fd);
      fwrite("\n", 1, strlen("\n"), fd);
      fclose(fd);

      args.rval().setBoolean(true);
    }
    else {
      args.rval().setBoolean(false);
    }
    free(filename);
    free(output);
  }
  return true;
}

void spidermonkey_state::replace_error(const char* m, unsigned int l, const char* os) {
      free_error();

      msg = new std::string(m);
      lineno = l;
      if(os)
        offending_source = new std::string(os);
      else
        offending_source = new std::string("<internally_generated>");
      error = true;
};

char* spidermonkey_state::error_to_json() {
      std::string *escaped_source = new std::string();
      bool escaped = false;
      for(char c : *offending_source) {
        if(c =='\\') {
          *escaped_source += c;
          escaped = true;
        }
        else {
          if((c == '"') && !escaped)
            *escaped_source += "\\\"";
          else
            *escaped_source += c;
          escaped = false;
        }
      }

      char fmt[] = "{\"lineno\": %d, \"message\": \"%s\", \"source\": \"%s\"}";
      size_t size = escaped_source->length() + msg->length() + strlen(fmt);
      char *retval = new char[size];

      snprintf(retval, size, fmt, lineno, msg->c_str(), escaped_source->c_str());
      delete escaped_source;

      free_error();
      return retval;
};

spidermonkey_vm::spidermonkey_vm(size_t thread_stack, uint32_t heap_size)
{

/* Bytes to allocate before GC */
#define MAX_GC_SIZE 1024 * 1024

      context = JS_NewContext(MAX_GC_SIZE);

      JS::InitSelfHostedCode(context);

      JS_SetNativeStackQuota(context, thread_stack);
      JS_SetGCParameter(context, JSGC_MAX_BYTES, heap_size);

      JS::ContextOptionsRef(context).setAsmJS(true);

      JS_SetGlobalJitCompilerOption(context, JSJitCompilerOption::JSJITCOMPILER_ION_ENABLE, true);
      JS_SetGlobalJitCompilerOption(context, JSJitCompilerOption::JSJITCOMPILER_BASELINE_ENABLE, true);

      JS::RealmOptions options;

      spidermonkey_state *state = new spidermonkey_state();

      // FIXME FIXME FIXME
      JS::RootedObject g(context, JS_NewGlobalObject(context, &global_class, nullptr, JS::FireOnNewGlobalHook, options));
      this->global = g;

      JSAutoRealm ar(context, g);
      JS_InitReflectParse(context, g);
      JS_DefineDebuggerObject(context, g);

      JS::SetWarningReporter(context, on_error);
      JS_AddInterruptCallback(context, on_branch);
      JS_SetContextPrivate(context, state);
      JS_DefineFunction(context, g, "ejsLog", (JSNative) js_log, 0, 0);
}

spidermonkey_vm::~spidermonkey_vm() {
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  JS_SetContextPrivate(this->context, nullptr);

  //Now we should be free to proceed with
  //freeing up memory without worrying about
  //crashing the VM.
  delete state;

  //delete global;
  JS_DestroyContext(this->context);
}

void spidermonkey_vm::sm_stop() {
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  state->terminate = true;
  JS_SetContextPrivate(this->context, state);

  // Request interrupt callback immediately. This call is thread-safe and can
  // be called outside of JS_BeginRequest/JS_EndRequest.
  JS_RequestInterruptCallback(this->context);
}

bool spidermonkey_vm::sm_eval(const char *filename, size_t filename_length, const char *code, size_t code_length, char** output, int handle_retval) {

  JSAutoRealm ar(this->context, this->global);

  char* filename0 = strndup(filename, filename_length);
  JS::CompileOptions options(this->context);
  options.setFileAndLine(filename0, 1);
  free(filename0);

  JS::SourceText<mozilla::Utf8Unit> source;
  if (!source.init(this->context, code, code_length, JS::SourceOwnership::Borrowed)) {
    //*output = state->error_to_json();
    return false;
  }

  JS::RootedScript script(this->context, JS::Compile(this->context, options, source));
  if (!script)
    this->check_js_exception();
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  if (state->error) {
    *output = state->error_to_json();
    JS_SetContextPrivate(this->context, state);
    return false;
  }

  JS::RootedValue result(this->context);
  if(!JS_ExecuteScript(this->context, script, &result))
    this->check_js_exception();
  state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  if (state->error) {
    *output = state->error_to_json();
    JS_SetContextPrivate(this->context, state);
    return false;
  }

  if (handle_retval) {
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
  if (JS_IsExceptionPending(this->context)) {
    JS::RootedValue exception_v(this->context);
    JS_GetPendingException(this->context, &exception_v);
    JS::RootedObject exception(this->context, &exception_v.toObject());
    JSErrorReport *report = JS_ErrorFromException(this->context, exception);
    on_error(this->context, report);
    JS_ClearPendingException(this->context);
  }
  return;
}
