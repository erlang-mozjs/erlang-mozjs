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

spidermonkey_vm *sm_initialize(long thread_stack, long heap_size) {
  if(!JS_IsInitialized())
    JS_Init();

/* Bytes to allocate before GC */
#define MAX_GC_SIZE 1024 * 1024

  spidermonkey_vm *vm = new spidermonkey_vm(MAX_GC_SIZE, thread_stack, heap_size, &global_class, on_error, on_branch, "ejsLog", (JSNative) js_log);

  return vm;
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
  //delete state; // FIXME FIXME FIXME

  //delete vm; // FIXME FIXME FIXME
}

void sm_poweron(void) {
  JS_Init();
}
void sm_shutdown(void) {
  JS_ShutDown();
}

const char* spidermonkey_vm::sm_eval(const char *filename, const char *code, int handle_retval) {
  if (code == nullptr)
      return nullptr;

  char *retval = nullptr;

  JS_BeginRequest(this->context);

  JSAutoCompartment ac(this->context, this->global);
  JSAutoRequest ar(this->context);

  JS::CompileOptions options(this->context);
  options
	  .setVersion(JSVERSION_LATEST)
	  .setUTF8(true)
          .setFileAndLine(filename, 1);

  JS::RootedScript script(this->context);
  bool ret = JS::Compile(this->context, options, code, strlen(code), &script);
  if (!ret && JS_IsExceptionPending(this->context)) {
    JS::RootedValue exception_v(this->context);
    JS_GetPendingException(this->context, &exception_v);
    JS::RootedObject exception(this->context, &exception_v.toObject());
    JSErrorReport *report = JS_ErrorFromException(this->context, exception);
    report->flags |= JSREPORT_EXCEPTION;
    on_error(this->context, report);
    JS_ClearPendingException(this->context);
  }

  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
  if (state->error) {
    retval = state->error_to_json();
    JS_SetContextPrivate(this->context, state);
  }
  else {
    JS::RootedValue result(this->context);
    JS_ExecuteScript(this->context, script, &result);
    state = (spidermonkey_state *) JS_GetContextPrivate(this->context);
    if (state->error) {
      retval = state->error_to_json();
      JS_SetContextPrivate(this->context, state);
    }
    else {
      if (handle_retval) {
        JS::RootedString str(this->context, JS::ToString(this->context, result));
        char *buf = JS_EncodeStringToUTF8(this->context, str);
        if (result.isString()) {
          size_t size = strlen(buf);
          retval = new char[size + 1];
          strncpy(retval, buf, size + 1);
        }
        else {
	  if(strcmp(buf, "undefined") == 0) {
            state->replace_error("Expression returned undefined");
            retval = state->error_to_json();
            JS_SetContextPrivate(this->context, state);
	  }
	  else {
            state->replace_error("non-JSON return value");
            retval = state->error_to_json();
            JS_SetContextPrivate(this->context, state);
	  }
        }
	JS_free(this->context, buf);
      }
    }
  }
  JS_EndRequest(this->context);

  return retval;
}
