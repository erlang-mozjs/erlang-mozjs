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
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <erl_driver.h>

#include "driver_comm.h"
#include "spidermonkey.h"

typedef struct _spidermonkey_error_t {
  unsigned int lineno;
  char *msg;
  char *offending_source;
} spidermonkey_error;

typedef struct _spidermonkey_state_t {
  int branch_count;
  spidermonkey_error *error;
  int terminate;
} spidermonkey_state;

void free_error(spidermonkey_state *state);

/* The class of the global object. */
static JSClass global_class = {
    "global", JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub, JS_DeletePropertyStub, JS_PropertyStub, JS_StrictPropertyStub,
    JS_EnumerateStub, JS_ResolveStub, JS_ConvertStub, NULL,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

char *copy_string(const char *source) {
  size_t size = strlen(source);
  char *retval = (char*)ejs_alloc(size + 1);
  strncpy(retval, source, size);
  retval[size] = '\0';
  return retval;
}

void on_error(JSContext *context, const char *message, JSErrorReport *report) {
  if (report->flags & JSREPORT_EXCEPTION) {
    spidermonkey_error *sm_error = (spidermonkey_error *)ejs_alloc(sizeof(spidermonkey_error));
    if (message != NULL) {
      sm_error->msg = copy_string(message);
    }
    else {
      sm_error->msg = copy_string("undefined error");
    }
    sm_error->lineno = report->lineno;
    if (report->linebuf != NULL) {
      sm_error->offending_source = copy_string(report->linebuf);
    }
    else {
      sm_error->offending_source = copy_string("unknown");
    }
    spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(context);
    state->error = sm_error;
    JS_SetContextPrivate(context, state);
  }
}

JSBool on_branch(JSContext *context) {
  JSBool return_value = JS_TRUE;
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(context);
  state->branch_count++;

  if (state->terminate)  {
      return_value = JS_FALSE;
  }
  else if (state->branch_count == 550) {
    JS_GC(JS_GetRuntime(context));
    state->branch_count = 0;
  }
  else if(state->branch_count % 100 == 0) {
    JS_MaybeGC(context);
  }

  return return_value;
}

JSBool js_log(JSContext *cx, unsigned argc, jsval *vp) {
  if (argc != 2) {
    JS_SET_RVAL(cx, vp, JSVAL_FALSE);
  }
  else {
    jsval *argv = JS_ARGV(cx, vp);
    jsval jsfilename = argv[0];
    jsval jsoutput = argv[1];
    char *filename = JS_EncodeString(cx, JS_ValueToString(cx, jsfilename));
    char *output = JS_EncodeString(cx, JS_ValueToString(cx, jsoutput));
    FILE *fd = fopen(filename, "a+");
    if (fd != NULL) {
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
      JS_SET_RVAL(cx, vp, JSVAL_TRUE);
    }
    else {
      JS_SET_RVAL(cx, vp, JSVAL_FALSE);
    }
    JS_free(cx, filename);
    JS_free(cx, output);
  }
  return JS_TRUE;
}

spidermonkey_vm *sm_initialize(long thread_stack, long heap_size) {
  spidermonkey_vm *vm = (spidermonkey_vm *)ejs_alloc(sizeof(spidermonkey_vm));
  spidermonkey_state *state = (spidermonkey_state *)ejs_alloc(sizeof(spidermonkey_state));
  state->branch_count = 0;
  state->error = NULL;
  state->terminate = 0;
  int gc_size = (int) heap_size * 0.25;

  vm->runtime = JS_NewRuntime(MAX_GC_SIZE, JS_USE_HELPER_THREADS);
  JS_SetNativeStackQuota(vm->runtime, thread_stack);
  JS_SetGCParameter(vm->runtime, JSGC_MAX_BYTES, heap_size);
  JS_SetGCParameter(vm->runtime, JSGC_MAX_MALLOC_BYTES, gc_size);

  vm->context = JS_NewContext(vm->runtime, 8192);

  JS_BeginRequest(vm->context);

  JS_SetOptions(vm->context, JSOPTION_VAROBJFIX);
  JS_SetOptions(vm->context, JSOPTION_EXTRA_WARNINGS);
  JS_SetOptions(vm->context, JSOPTION_COMPILE_N_GO);
  JS_SetOptions(vm->context, JSVERSION_LATEST);

  vm->global = JS_NewGlobalObject(vm->context, &global_class, NULL);
  JSAutoCompartment ac(vm->context, vm->global);
  JS_InitStandardClasses(vm->context, vm->global);
  JS_SetErrorReporter(vm->context, on_error);
  JS_SetOperationCallback(vm->context, on_branch);
  JS_SetContextPrivate(vm->context, state);
  JSNative funptr = (JSNative) js_log;
  JS_DefineFunction(vm->context, js::GetDefaultGlobalForContext(vm->context), "ejsLog", funptr,
                    0, 0);
  JS_EndRequest(vm->context);

  return vm;
}

void sm_stop(spidermonkey_vm *vm) {
  vm->global = NULL;
  JS_BeginRequest(vm->context);
  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(vm->context);
  state->terminate = 1;
  JS_SetContextPrivate(vm->context, state);

  //Wait for any executing function to stop
  //before beginning to free up any memory.
  while (JS_IsRunning(vm->context))  {
      sleep(1);
  }

  JS_EndRequest(vm->context);

  //Now we should be free to proceed with
  //freeing up memory without worrying about
  //crashing the VM.
  if (state != NULL) {
    if (state->error != NULL) {
      free_error(state);
    }
    driver_free(state);
  }
  //JS_SetContextPrivate(vm->context, NULL);
  //JS_DestroyContext(vm->context);
  JS_DestroyRuntime(vm->runtime);
  driver_free(vm);
}

void sm_shutdown(void) {
  JS_ShutDown();
}

char *escape_quotes(char *text) {
  size_t bufsize = strlen(text) * 2;
  char *buf = (char*)ejs_alloc(bufsize);
  memset(buf, 0, bufsize);
  int i = 0;
  int x = 0;
  int escaped = 0;
  for (i = 0; i < (int)strlen(text); i++) {
    if (text[i] == '"') {
      if(!escaped) {
        memcpy(&buf[x], (char *) "\\\"", 2);
        x += 2;
      }
      else {
        memcpy(&buf[x], &text[i], 1);
        x++;
      }
    }
    else {
      if(text[i] =='\\') {
        escaped = 1;
      }
      else {
        escaped = 0;
      }
      memcpy(&buf[x], &text[i], 1);
      x++;
    }
  }
  size_t buf_size = strlen(buf);
  char *retval = (char*)ejs_alloc(buf_size + 1);
  strncpy(retval, buf, buf_size);
  retval[buf_size] = '\0';
  driver_free(buf);
  return retval;
}

char *error_to_json(const spidermonkey_error *error) {
  char *escaped_source = escape_quotes(error->offending_source);
  /* size = length(escaped source) + length(error msg) + JSON formatting */
  size_t size = strlen(escaped_source) + strlen(error->msg) + 80;
  char *retval = (char*)ejs_alloc(size);

  snprintf(retval, size, "{\"error\": {\"lineno\": %d, \"message\": \"%s\", \"source\": \"%s\"}}",
           error->lineno, error->msg, escaped_source);
  driver_free(escaped_source);
  return retval;
}

void free_error(spidermonkey_state *state) {
  driver_free(state->error->offending_source);
  driver_free(state->error->msg);
  driver_free(state->error);
  state->error = NULL;
}

char *sm_eval(spidermonkey_vm *vm, const char *filename, const char *code, int handle_retval) {
  char *retval = NULL;
  JSScript *script;
  jsval result;

  if (code == NULL) {
      return NULL;
  }


    JSAutoCompartment ac(vm->context, vm->global);
    JSAutoRequest ar(vm->context);

  JS_BeginRequest(vm->context);

//  script = JS_CompileScript(vm->context,
  //                        vm->global,
    //                    code, strlen(code),
      //                filename, 1);

    JS::RootedObject obj(vm->context, vm->global);
    JS::CompileOptions options(vm->context);
    options.setUTF8(true).setFileAndLine(filename, 1);

    script = JS::Compile(vm->context, obj, options, code, strlen(code));


  spidermonkey_state *state = (spidermonkey_state *) JS_GetContextPrivate(vm->context);
  if (state->error == NULL) {
    JS_ClearPendingException(vm->context);
    JS_ExecuteScript(vm->context, vm->global, script, &result);
    state = (spidermonkey_state *) JS_GetContextPrivate(vm->context);
    if (state->error == NULL) {
      if (handle_retval) {
        if (JSVAL_IS_STRING(result)) {
          JSString *str = JS_ValueToString(vm->context, result);
          char *buf = JS_EncodeStringToUTF8(vm->context, str);
          retval = copy_string(buf);
          JS_free(vm->context, buf);
        }
        else {
          char *tmp = JS_EncodeStringToUTF8(vm->context, JS_ValueToString(vm->context, result));
	  if(strcmp(tmp, "undefined") == 0) {
            retval = copy_string("{\"error\": \"Expression returned undefined\", \"lineno\": 0, \"source\": \"unknown\"}");
	  }
	  else {
            retval = copy_string("{\"error\": \"non-JSON return value\", \"lineno\": 0, \"source\": \"unknown\"}");
	  }
	  JS_free(vm->context, tmp);
        }
      }
    }
    else {
      retval = error_to_json(state->error);
      free_error(state);
      JS_SetContextPrivate(vm->context, state);
    }
  }
  else {
    retval = error_to_json(state->error);
    free_error(state);
    JS_SetContextPrivate(vm->context, state);
  }
  JS_EndRequest(vm->context);
  return retval;
}
