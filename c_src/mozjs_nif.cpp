/*
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include "erl_nif.h"
#include "spidermonkey.h"

/*
void* operator new(size_t size)
{
  void *p = enif_alloc(size);
  if (p)
    return p;
  else
    erts_exit(1, "erlang_js: Can't allocate %lu bytes of memory\n", size);
    // throw std::bad_alloc();
};

void operator delete(void* ptr) noexcept
{
     enif_free(ptr);
};
*/

static ErlNifResourceType* mozjs_RESOURCE = nullptr;

struct mozjs_handle
{
    spidermonkey_vm* vm = nullptr;
};

// Prototypes
static ERL_NIF_TERM mozjs_init(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mozjs_eval(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mozjs_stop(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"sm_init", 2, mozjs_init},
    {"sm_eval_nif", 4, mozjs_eval},
    {"sm_stop", 1, mozjs_stop}
};

static ERL_NIF_TERM mozjs_init(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    mozjs_handle* handle = (mozjs_handle*)enif_alloc_resource(mozjs_RESOURCE,
                                                    sizeof(mozjs_handle));
    size_t thread_stack = 0;
    uint32_t heap_size = 0;
    enif_get_uint(env, argv[0], (unsigned int*)&thread_stack);
    enif_get_uint(env, argv[1], &heap_size);
    handle->vm = new spidermonkey_vm(thread_stack * (1024 * 1024), heap_size * (1024 * 1024));

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM mozjs_eval(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    mozjs_handle* handle = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, (void**)&handle))
        return enif_make_badarg(env);

    unsigned filename_length = 0;
    enif_get_list_length(env, argv[1], &filename_length);
    char *filename = new char[filename_length+1];
    enif_get_string(env, argv[1], filename, filename_length+1, ERL_NIF_LATIN1);

    unsigned code_length = 0;
    enif_get_list_length(env, argv[2], &code_length);
    char *code = new char[code_length+1];
    enif_get_string(env, argv[2], code, code_length+1, ERL_NIF_LATIN1);

    int handle_retval = 0;
    enif_get_int(env, argv[3], &handle_retval);

    char* output = nullptr;
    bool retval = handle->vm->sm_eval(filename, code, &output, handle_retval);

    delete[] filename;
    delete[] code;

    if (output) {
        ErlNifBinary result;
        enif_alloc_binary(strlen(output), &result);
        memcpy((char*)result.data, output, result.size);
        delete[] output;

	if (retval)
	    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &result));
	else
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_binary(env, &result));
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM mozjs_stop(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    mozjs_handle* handle = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, (void**)&handle))
        return enif_make_badarg(env);

    handle->vm->sm_stop();

    return enif_make_atom(env, "ok");
}

static void mozjs_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in mozjs_handle */
    mozjs_handle* handle = (mozjs_handle*)arg;
    delete handle->vm;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ErlNifResourceFlags(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    ErlNifResourceType* rt = enif_open_resource_type(env, nullptr,
                                                     "mozjs_resource",
                                                     &mozjs_resource_cleanup,
                                                     flags, nullptr);
    if (rt == nullptr)
        return -1;

    mozjs_RESOURCE = rt;

    JS_Init();

    return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
    JS_ShutDown();
    return;
}

static int on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_INIT(mozjs_nif, nif_funcs, &on_load, nullptr, on_upgrade, on_unload);