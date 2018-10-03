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
#include <js/Conversions.h>

#include "spidermonkey.h"

/* The class of the global object. */
static constexpr JSClass global_class = {
    "global", JSCLASS_GLOBAL_FLAGS,
    nullptr, nullptr, nullptr, nullptr
};

void on_error(JSContext *context, JSErrorReport *report) {
  if (report->flags & JSREPORT_EXCEPTION) {
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
    char *filename = JS_EncodeString(cx, JS::ToString(cx, args[0]));
    char *output = JS_EncodeString(cx, JS::ToString(cx, args[1]));

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
    JS_free(cx, filename);
    JS_free(cx, output);
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

      uint32_t gc_size = (uint32_t) heap_size * 0.25;
      context = JS_NewContext(MAX_GC_SIZE);

      JS::InitSelfHostedCode(context);

      JS_SetNativeStackQuota(context, thread_stack);
      JS_SetGCParameter(context, JSGC_MAX_BYTES, heap_size);
      JS_SetGCParameter(context, JSGC_MAX_MALLOC_BYTES, gc_size);

      JS::ContextOptionsRef(context)
          .setIon(true)
          .setBaseline(true)
          .setAsmJS(true)
          .setExtraWarnings(true);

      JS_BeginRequest(context);

      JS::CompartmentOptions options;

      spidermonkey_state *state = new spidermonkey_state();

      // FIXME FIXME FIXME
      JS::RootedObject g(context, JS_NewGlobalObject(context, &global_class, nullptr, JS::FireOnNewGlobalHook, options));
      this->global = g;

      JSAutoCompartment ac(context, g);
      JS_InitStandardClasses(context, g);
      JS_InitReflectParse(context, g);
      JS_DefineDebuggerObject(context, g);

      JS::SetWarningReporter(context, on_error);
      JS_AddInterruptCallback(context, on_branch);
      JS_SetContextPrivate(context, state);
      JS_DefineFunction(context, g, "ejsLog", (JSNative) js_log, 0, 0);
      JS_EndRequest(context);
}

spidermonkey_vm::~spidermonkey_vm() {
  //delete global;
  if(context){
    JS_DestroyContext(context);
    context = nullptr;
  }
}

void spidermonkey_vm::sm_stop() {
  JS_BeginRequest(this->context);
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  state->terminate = true;
  JS_SetContextPrivate(this->context, state);

  //Wait for any executing function to stop
  //before beginning to free up any memory.
  while (JS_IsRunning(this->context))
      sleep(1);

  JS_GC(this->context);

  JS_SetContextPrivate(this->context, nullptr);
  JS_EndRequest(this->context);

  //Now we should be free to proceed with
  //freeing up memory without worrying about
  //crashing the VM.
  delete state;
}

bool spidermonkey_vm::sm_eval(const char *filename, size_t filename_length, const char *code, size_t code_length, char** output, int handle_retval) {
  if (code == nullptr)
      return false;

  JS_BeginRequest(this->context);

  JSAutoCompartment ac(this->context, this->global);
  JSAutoRequest ar(this->context);

  char* filename0 = strndup(filename, filename_length);
  JS::CompileOptions options(this->context);
  options
	  .setUTF8(true)
          .setFileAndLine(filename0, 1);
  free(filename0);

  JS::RootedScript script(this->context);
  if (!JS::Compile(this->context, options, code, code_length, &script))
    this->check_js_exception();
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  if (state->error) {
    *output = state->error_to_json();
    JS_SetContextPrivate(this->context, state);
    JS_EndRequest(this->context);
    return false;
  }

  JS::RootedValue result(this->context);
  if(!JS_ExecuteScript(this->context, script, &result))
    this->check_js_exception();
  state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  if (state->error) {
    *output = state->error_to_json();
    JS_SetContextPrivate(this->context, state);
    JS_EndRequest(this->context);
    return false;
  }

  if (handle_retval) {
    JS::RootedString str(this->context, JS::ToString(this->context, result));
    char *buf = JS_EncodeStringToUTF8(this->context, str);
    size_t size = strlen(buf);
    *output = new char[size + 1];
    strncpy(*output, buf, size + 1);
    JS_free(this->context, buf);
  }
  JS_EndRequest(this->context);

  return true;
}

void spidermonkey_vm::check_js_exception()
{
  if (JS_IsExceptionPending(this->context)) {
    JS::RootedValue exception_v(this->context);
    JS_GetPendingException(this->context, &exception_v);
    JS::RootedObject exception(this->context, &exception_v.toObject());
    JSErrorReport *report = JS_ErrorFromException(this->context, exception);
    report->flags |= JSREPORT_EXCEPTION;
    on_error(this->context, report);
    JS_ClearPendingException(this->context);
  }
  return;
}
