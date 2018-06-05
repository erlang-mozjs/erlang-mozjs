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

#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <erl_driver.h>

#include "driver_comm.h"
#include "spidermonkey.h"

#include <string>

void* operator new(size_t size)
{
  void *p = driver_alloc((ErlDrvSizeT)size);
  if (p)
    return p;
  else
    erts_exit(1, "erlang_js: Can't allocate %lu bytes of memory\n", size);
    // throw std::bad_alloc();
};

void operator delete(void* ptr) noexcept
{
  driver_free(ptr);
};

class spidermonkey_state {
  public:
    int branch_count = 0;
    bool terminate = false;
    bool error = false;
    spidermonkey_state() {};
    ~spidermonkey_state()
    {
      free_error();
    };
    void replace_error(const char* m = "undefined error", unsigned int l = 0, const char* os = "<unknown>")
    {
      free_error();

      msg = new std::string(m);
      lineno = l;
      offending_source = new std::string(os);
      error = true;
    };
    char* error_to_json()
    {
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

      char fmt[] = "{\"error\": {\"lineno\": %d, \"message\": \"%s\", \"source\": \"%s\"}}";
      size_t size = escaped_source->length() + msg->length() + strlen(fmt);
      char *retval = new char[size];

      snprintf(retval, size, fmt, lineno, msg->c_str(), escaped_source->c_str());
      delete escaped_source;

      free_error();
      return retval;
    };
  private:
    unsigned int lineno = 0;
    std::string *msg = nullptr;
    std::string *offending_source = nullptr;
    void free_error()
    {
      if(error){
        error = false;
        delete msg;
        delete offending_source;
      }
    };
};

/* The class of the global object. */
static JSClass global_class = {
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

spidermonkey_vm *sm_initialize(long thread_stack, long heap_size) {
  spidermonkey_vm *vm = (spidermonkey_vm *)ejs_alloc(sizeof(spidermonkey_vm));
  spidermonkey_state *state = new spidermonkey_state();
  int gc_size = (int) heap_size * 0.25;

  if(!JS_IsInitialized())
    JS_Init();

/* Bytes to allocate before GC */
#define MAX_GC_SIZE 1024 * 1024

  vm->context = JS_NewContext(MAX_GC_SIZE);
  JS_SetNativeStackQuota(vm->context, thread_stack);
  JS_SetGCParameter(vm->context, JSGC_MAX_BYTES, heap_size);
  JS_SetGCParameter(vm->context, JSGC_MAX_MALLOC_BYTES, gc_size);

  JS::ContextOptionsRef(vm->context)
          .setIon(true)
          .setBaseline(true)
          .setAsmJS(true)
	  .setExtraWarnings(true);

  JS_BeginRequest(vm->context);

  JS::InitSelfHostedCode(vm->context);

  JS::CompartmentOptions options;
  options.behaviors().setVersion(JSVERSION_LATEST);

  vm->global = JS::RootedObject(vm->context, JS_NewGlobalObject(vm->context, &global_class, nullptr, JS::FireOnNewGlobalHook, options));

  JSAutoCompartment ac(vm->context, vm->global);
  JS_InitStandardClasses(vm->context, vm->global);
  JS_InitReflectParse(vm->context, vm->global);
  JS_DefineDebuggerObject(vm->context, vm->global);
  JS::SetWarningReporter(vm->context, on_error);
  JS_AddInterruptCallback(vm->context, on_branch);
  JS_SetContextPrivate(vm->context, state);
  JSNative funptr = (JSNative) js_log;
  JS_DefineFunction(vm->context, vm->global, "ejsLog", funptr,
                    0, 0);
  JS_EndRequest(vm->context);

  return vm;
}

void sm_stop(spidermonkey_vm *vm) {
  vm->global = nullptr;

  JS_BeginRequest(vm->context);
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(vm->context);
  state->terminate = true;
  JS_SetContextPrivate(vm->context, state);

  //Wait for any executing function to stop
  //before beginning to free up any memory.
  while (JS_IsRunning(vm->context))
      sleep(1);

  JS_EndRequest(vm->context);

  //Now we should be free to proceed with
  //freeing up memory without worrying about
  //crashing the VM.
  delete state;
  JS_SetContextPrivate(vm->context, NULL);
  if(vm->context){
    // FIXME FIXME FIXME called from wrong thread. Switch to NIF?
    // JS_DestroyContext(vm->context);
    // vm->context = nullptr;
  }
  driver_free(vm);
}

void sm_shutdown(void) {
  JS_ShutDown();
}

const char *sm_eval(spidermonkey_vm *vm, const char *filename, const char *code, int handle_retval) {
  if (code == nullptr)
      return nullptr;

  char *retval = nullptr;

  JS_BeginRequest(vm->context);

  JSAutoCompartment ac(vm->context, vm->global);
  JSAutoRequest ar(vm->context);

  JS::CompileOptions options(vm->context);
  options
	  .setVersion(JSVERSION_LATEST)
	  .setUTF8(true)
          .setFileAndLine(filename, 1);

  JS::RootedScript script(vm->context);
  bool ret = JS::Compile(vm->context, options, code, strlen(code), &script);
  if (!ret && JS_IsExceptionPending(vm->context)) {
    JS::RootedValue exception_v(vm->context);
    JS_GetPendingException(vm->context, &exception_v);
    JS::RootedObject exception(vm->context, &exception_v.toObject());
    JSErrorReport *report = JS_ErrorFromException(vm->context, exception);
    report->flags |= JSREPORT_EXCEPTION;
    on_error(vm->context, report);
    JS_ClearPendingException(vm->context);
  }

  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(vm->context);
  if (state->error) {
    retval = state->error_to_json();
    JS_SetContextPrivate(vm->context, state);
  }
  else {
    JS::RootedValue result(vm->context);
    JS_ExecuteScript(vm->context, script, &result);
    state = (spidermonkey_state *) JS_GetContextPrivate(vm->context);
    if (state->error) {
      retval = state->error_to_json();
      JS_SetContextPrivate(vm->context, state);
    }
    else {
      if (handle_retval) {
        JS::RootedString str(vm->context, JS::ToString(vm->context, result));
        char *buf = JS_EncodeStringToUTF8(vm->context, str);
        if (result.isString()) {
          size_t size = strlen(buf);
          retval = new char[size + 1];
          strncpy(retval, buf, size + 1);
        }
        else {
	  if(strcmp(buf, "undefined") == 0) {
            state->replace_error("Expression returned undefined");
            retval = state->error_to_json();
            JS_SetContextPrivate(vm->context, state);
	  }
	  else {
            state->replace_error("non-JSON return value");
            retval = state->error_to_json();
            JS_SetContextPrivate(vm->context, state);
	  }
        }
	JS_free(vm->context, buf);
      }
    }
  }
  JS_EndRequest(vm->context);

  return retval;
}
