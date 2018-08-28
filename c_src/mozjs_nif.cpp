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

#include <js/Initialization.h>

static ErlNifResourceType* mozjs_RESOURCE = nullptr;

ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_noinit;

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

    return enif_make_tuple2(env, atom_ok, result);
}

static ERL_NIF_TERM mozjs_eval(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    mozjs_handle* handle = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, (void**)&handle))
        return enif_make_badarg(env);

    if (handle->vm == nullptr)
        return enif_make_tuple2(env, atom_error, atom_noinit);

    ErlNifBinary filename, code;

    if(!enif_inspect_binary(env, argv[1], &filename))
        return enif_make_badarg(env);

    if(!enif_inspect_binary(env, argv[2], &code))
        return enif_make_badarg(env);

    int handle_retval = 0;
    enif_get_int(env, argv[3], &handle_retval);

    char* output = nullptr;
    bool retval = handle->vm->sm_eval((const char*)filename.data, filename.size, (const char*)code.data, code.size, &output, handle_retval);

    if (output) {
        ErlNifBinary result;
        enif_alloc_binary(strlen(output), &result);
        memcpy((char*)result.data, output, result.size);
        delete[] output;

	if (retval)
	    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &result));
	else
	    return enif_make_tuple2(env, atom_error, enif_make_binary(env, &result));
    }

    return atom_ok;
}

static ERL_NIF_TERM mozjs_stop(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    mozjs_handle* handle = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, (void**)&handle))
        return enif_make_badarg(env);

    if (handle->vm == nullptr)
        return enif_make_tuple2(env, atom_error, atom_noinit);

    handle->vm->sm_stop();
    delete handle->vm;
    handle->vm = nullptr;

    return atom_ok;
}

static void mozjs_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in mozjs_handle */
    mozjs_handle* handle = (mozjs_handle*)arg;
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

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_noinit = enif_make_atom(env, "mozjs_not_initialized");

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
