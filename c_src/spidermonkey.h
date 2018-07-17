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

#ifndef __SPIDERMONKEY_INTERFACE_
#define __SPIDERMONKEY_INTERFACE_

#include <jsapi.h>
#include <jsfriendapi.h>
#include <js/Conversions.h>
#include <js/Value.h>
#include <js/Initialization.h>

extern "C" void erts_exit(int n, const char*, ...);

#include <string>
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

class spidermonkey_vm {
  public:
    JSContext* context;
    JSObject* global;
    spidermonkey_vm(
                      uint32_t GC_Size,
                      size_t thread_stack,
                      uint32_t heap_size,
                      JSClass* global_class,
                      JS::WarningReporter on_error,
                      JSInterruptCallback on_branch,
                      const char* funname, JSNative funptr)
    {
      uint32_t gc_size = (uint32_t) heap_size * 0.25;
      context = JS_NewContext(GC_Size);

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
      options.behaviors().setVersion(JSVERSION_LATEST);

      spidermonkey_state *state = new spidermonkey_state();

      // FIXME FIXME FIXME
      JS::RootedObject g(context, JS_NewGlobalObject(context, global_class, nullptr, JS::FireOnNewGlobalHook, options));
      global = g;

      JSAutoCompartment ac(context, g);
      JS_InitStandardClasses(context, g);
      JS_InitReflectParse(context, g);
      JS_DefineDebuggerObject(context, g);

      JS::SetWarningReporter(context, on_error);
      JS_AddInterruptCallback(context, on_branch);
      JS_SetContextPrivate(context, state);
      JS_DefineFunction(context, g, funname, funptr, 0, 0);
      JS_EndRequest(context);
    };
    ~spidermonkey_vm()
    {
      //delete global;
      if(context){
        //JS_DestroyContext(context);
        //delete context;
        context = nullptr;
      }
    };

    const char *sm_eval(const char *filename, const char *code, int handle_retval);
    void sm_stop();

    void* operator new(size_t size)
    {
      void *p = driver_alloc((ErlDrvSizeT)size);
      if (p)
        return p;

      erts_exit(1, "erlang-mozjs: Can't allocate %lu bytes of memory\n", size);
      // throw std::bad_alloc();
    };

    void operator delete(void* ptr) noexcept
    {
      driver_free(ptr);
    };
};

spidermonkey_vm *sm_initialize(long thread_stack, long heap_size);
void sm_poweron();

void sm_shutdown(void);

#endif
