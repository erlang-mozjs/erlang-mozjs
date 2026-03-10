// SPDX-FileCopyrightText: 2009-2010 Basho Technologies
// SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
// SPDX-License-Identifier: Apache-2.0

#include <cstdlib>

#include "erl_nif.h"

#include <js/Initialization.h>

#include "spidermonkey.h"

static ErlNifResourceType* mozjs_RESOURCE = nullptr;

ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_noinit;

struct mozjs_handle
{
    spidermonkey_vm* vm = nullptr;
    ErlNifMutex* mtx = nullptr;
};

// Prototypes
static ERL_NIF_TERM mozjs_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mozjs_eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mozjs_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mozjs_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"sm_init", 2, mozjs_init, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"sm_eval_nif", 4, mozjs_eval, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"sm_cancel", 1, mozjs_cancel, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"sm_stop", 1, mozjs_stop, ERL_NIF_DIRTY_JOB_CPU_BOUND}};

static ERL_NIF_TERM mozjs_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    auto* handle =
        static_cast<mozjs_handle*>(enif_alloc_resource(mozjs_RESOURCE, sizeof(mozjs_handle)));
    handle->mtx = enif_mutex_create("mozjs_handle");
    unsigned int thread_stack = 0;
    uint32_t heap_size = 0;
    enif_get_uint(env, argv[0], &thread_stack);
    enif_get_uint(env, argv[1], &heap_size);
    handle->vm = new spidermonkey_vm(static_cast<size_t>(thread_stack) * (1024 * 1024),
                                     heap_size * (1024 * 1024));

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);

    return enif_make_tuple2(env, atom_ok, result);
}

static ERL_NIF_TERM mozjs_eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    void* handle_ptr = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, &handle_ptr))
        return enif_make_badarg(env);
    auto* handle = static_cast<mozjs_handle*>(handle_ptr);

    ErlNifBinary filename, code;

    if (!enif_inspect_binary(env, argv[1], &filename))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[2], &code))
        return enif_make_badarg(env);

    int handle_retval = 0;
    enif_get_int(env, argv[3], &handle_retval);

    enif_mutex_lock(handle->mtx);

    if (handle->vm == nullptr)
    {
        enif_mutex_unlock(handle->mtx);
        return enif_make_tuple2(env, atom_error, atom_noinit);
    }

    char* output = nullptr;
    bool retval = handle->vm->sm_eval(reinterpret_cast<const char*>(filename.data), filename.size,
                                      reinterpret_cast<const char*>(code.data), code.size, &output,
                                      handle_retval);

    enif_mutex_unlock(handle->mtx);

    if (output)
    {
        ErlNifBinary bin_result;
        enif_alloc_binary(strlen(output), &bin_result);
        memcpy(bin_result.data, output, bin_result.size);
        delete[] output;

        if (retval)
            return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &bin_result));
        else
            return enif_make_tuple2(env, atom_error, enif_make_binary(env, &bin_result));
    }

    if (!retval)
        return enif_make_tuple2(env, atom_error,
                                enif_make_atom(env, "unknown_error"));

    return atom_ok;
}

static ERL_NIF_TERM mozjs_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    void* handle_ptr = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, &handle_ptr))
        return enif_make_badarg(env);
    auto* handle = static_cast<mozjs_handle*>(handle_ptr);

    if (handle->vm == nullptr)
        return enif_make_tuple2(env, atom_error, atom_noinit);

    handle->vm->sm_stop();

    return atom_ok;
}

static ERL_NIF_TERM mozjs_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    void* handle_ptr = nullptr;

    if (!enif_get_resource(env, argv[0], mozjs_RESOURCE, &handle_ptr))
        return enif_make_badarg(env);
    auto* handle = static_cast<mozjs_handle*>(handle_ptr);

    enif_mutex_lock(handle->mtx);

    if (handle->vm == nullptr)
    {
        enif_mutex_unlock(handle->mtx);
        return enif_make_tuple2(env, atom_error, atom_noinit);
    }

    handle->vm->sm_stop();
    delete handle->vm;
    handle->vm = nullptr;

    enif_mutex_unlock(handle->mtx);

    return atom_ok;
}

static void mozjs_resource_cleanup(ErlNifEnv* env, void* arg)
{
    (void)env;
    auto* handle = static_cast<mozjs_handle*>(arg);

    if (handle->mtx != nullptr)
    {
        enif_mutex_lock(handle->mtx);

        if (handle->vm != nullptr)
        {
            handle->vm->sm_stop();
            delete handle->vm;
            handle->vm = nullptr;
        }

        enif_mutex_unlock(handle->mtx);
        enif_mutex_destroy(handle->mtx);
        handle->mtx = nullptr;
    }
}

/* Registered via atexit() right after JS_Init().  Because atexit handlers
   run in LIFO order, this fires BEFORE the handler that JS_Init() itself
   registered.  By the time we get here the BEAM has already shut down its
   schedulers, so no NIF calls are in flight and all JS contexts created
   via sm_init have been destroyed by their respective sm_stop / resource
   destructor calls.  Calling JS_ShutDown() here joins SpiderMonkey's
   internal helper threads (GC, compilation) cleanly.  The subsequent
   atexit handler from JS_Init() sees that shutdown already happened and
   becomes a no-op. */
static void mozjs_atexit(void)
{
    JS_ShutDown();
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;
    ErlNifResourceFlags flags =
        ErlNifResourceFlags(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    ErlNifResourceType* rt = enif_open_resource_type(
        env, nullptr, "mozjs_resource", &mozjs_resource_cleanup, flags, nullptr);
    if (rt == nullptr)
        return -1;

    mozjs_RESOURCE = rt;

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_noinit = enif_make_atom(env, "mozjs_not_initialized");

    JS_Init();
    std::atexit(mozjs_atexit);

    return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
    (void)env;
    (void)priv_data;
}

static int on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv;
    (void)old_priv;
    (void)load_info;
    return 0;
}

ERL_NIF_INIT(mozjs_nif, nif_funcs, &on_load, nullptr, on_upgrade, on_unload);
