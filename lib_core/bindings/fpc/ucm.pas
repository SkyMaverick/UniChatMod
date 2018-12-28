{ UniChatMod Free Pascal binding.
  Only for create GUI application (not plugin). Port only UniAPI.

  Copyright (c) 2019 SkyMaverick

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit ucm;

interface

const
{$IFDEF UNIX}
    UCMConst_LibName = 'libucm.so';
{$ELSE}
    UCMConst_LibName = 'libucm.dll';
{$ENDIF}

    UCMStartProcName = 'ucm_core_start';
    UCMStopProcName = 'ucm_core_stop';
    UCMInfoProcName = 'ucm_core_info';


type

    //TODO remove hardcode
    TUCMPath = array [0..4096] of char;

    // ######################################################################
    //  ENUMERATIONS DECLARE
    // ######################################################################

    TUCMRetStatus = (
        UCM_RET_SUCCESS,
        UCM_RET_UNKNOWERROR,
        UCM_RET_WRONGPARAM,
        UCM_RET_EXCEPTION,
        UCM_RET_UBOUND,
        UCM_RET_UNREALIZED,
        UCM_RET_NONALLOC,
        UCM_RET_NOOBJECT,
        UCM_RET_NOACCESS,
        UCM_RET_OVERFLOW,
        UCM_RET_EMPTY,
        UCM_RET_DUPLICATE,
        UCM_RET_BUSY);

    TUCMLogType = (
        UCM_LOG_INFO = 1,
        UCM_LOG_DEBUG = 2,
        UCM_LOG_ERROR = 4);

    // ######################################################################
    //  PLUGINS FUNCTIOANLITY
    // ######################################################################

    TUCMVAPI = packed record
        vmajor: byte;
        vminor: byte;
    end;

    TUCMPluginInfo = packed record
        api: TUCMVAPI;
        sys: byte;
        vmajor: word;
        vminor: word;
        vpatch: word;
        flags: longword;
        build: packed record
            commit: PWideChar;
            datetime: PWideChar;
            target: PWideChar;
            compiler: PWideChar;
            options: PWideChar;
            flags: PWideChar;
        end;
        pid: PWideChar;

        Name: PWideChar;
        developer: PWideChar;
        description: PWideChar;
        copyright: PWideChar;
        email: PWideChar;
        website: PWideChar;
    end;
    PUCMPluginInfo = ^TUCMPluginInfo;

    TUCMCB_PlugProc = procedure(); cdecl;
    TUCMCB_PlugFunc = function(): TUCMRetStatus; cdecl;
    TUCMCB_PlugMessage = procedure(id: cardinal; ctx: UIntPtr;
        x1: cardinal; x2: cardinal); cdecl;

    TUCMPlugin = packed record
        info: TUCMPluginInfo;
        run: TUCMCB_PlugFunc;
        stop: TUCMCB_PlugFunc;
        message: TUCMCB_PlugMessage;
    end;
    PUCMPlugin = ^TUCMPlugin;
    PPUCMPlugin = ^PUCMPlugin;

    // ######################################################################
    //      CHAT FUNCTIONALITY API IMPLEMENTATION
    // ######################################################################

    TUCMEvent = packed record
        ev: byte;
        size: QWord;
        Sender: Pointer;
    end;
    PUCMEvent = ^TUCMEvent;
    PPUCMEvent = ^PUCMEvent;

    // ######################################################################
    //      CHAT FUNCTIONALITY API IMPLEMENTATION
    // ######################################################################

    TUCMGui = packed record
        core: TUCMPlugin;
        // TODO
    end;

    // ######################################################################
    //  API FUNCTIONALITY
    // ######################################################################
    
    PPointer = ^Pointer;
    TUCMCB_MemAlloc = function(size: cardinal): Pointer; cdecl;
    TUCMCB_MemZeroAlloc = function(size: cardinal): Pointer; cdecl;
    TUCMCB_MemCellAlloc = function(nmem: cardinal; size: cardinal): Pointer; cdecl;
    TUCMCB_MemFree = procedure(obj: Pointer); cdecl;
    TUCMCB_MemZero = procedure(mem: Pointer; size: cardinal); cdecl;
    TUCMCB_MemRealloc = function(mem: PPointer; size: cardinal): integer; cdecl;
    TUCMCB_StrDup = function(const str: PChar): PChar; cdecl;

{$IFDEF UNIX}
    TUCMCB_ThreadFunc = procedure(ctx: Pointer); cdecl;
{$ELSE}
    TUCMCB_ThreadFunc = procedure(ctx: Pointer); stdcall;
{$ENDIF}
    TUCMCB_ThreadCreate = function(func: TUCMCB_ThreadFunc;
        ctx: Pointer): UIntPtr; cdecl;
    TUCMCB_ThreadOperation = function(tid: UIntPtr): integer; cdecl;
    TUCMCB_ThreadExit = procedure(ret: Pointer); cdecl;

    TUCMCB_MutexCreate = function(): UIntPtr; cdecl;
    TUCMCB_MutexFree = procedure(_mtx: UIntPtr); cdecl;
    TUCMCB_MutexLock = function(_mtx: UIntPtr): integer; cdecl;
    TUCMCB_MutexUnlock = function(_mtx: UIntPtr): integer; cdecl;

    TUCMCB_CondCreate = function(): UIntPtr; cdecl;
    TUCMCB_CondFree = procedure(_cond: UIntPtr) cdecl;
    TUCMCB_CondOperation = function(_cond: UIntPtr): integer; cdecl;

    PSize = ^cardinal;
    TUCMCB_StoreGetInt = function(obj: PUCMPlugin; key: PChar;
        def: integer): integer; cdecl;
    TUCMCB_StoreGetLongInt = function(obj: PUCMPlugin; key: PChar;
        def: longint): longint; cdecl;
    TUCMCB_StoreGetSingle = function(obj: PUCMPlugin; key: PChar;
        def: single): single; cdecl;
    TUCMCB_StoreGetPChar = function(obj: PUCMPlugin; key: PChar;
        def: PChar): PChar; cdecl;
    TUCMCB_StoreGetPWideChar = function(obj: PUCMPlugin; key: PChar;
        def: PWideChar): PWideChar; cdecl;
    TUCMCB_StoreGetBlob = function(obj: PUCMPlugin; key: PChar;
        size: PSize): UIntPtr; cdecl;
    TUCMCB_StoreSetInt = procedure(obj: PUCMPlugin; key: PChar;
        Value: integer); cdecl;
    TUCMCB_StoreSetLongInt = procedure(obj: PUCMPlugin; key: PChar;
        Value: longint); cdecl;
    TUCMCB_StoreSetSingle = procedure(obj: PUCMPlugin; key: PChar;
        Value: single); cdecl;
    TUCMCB_StoreSetPChar = procedure(obj: PUCMPlugin; key: PChar;
        Value: PChar); cdecl;
    TUCMCB_StoreSetPWideChar = procedure(obj: PUCMPlugin; key: PChar;
        Value: PWideChar); cdecl;
    TUCMCB_StoreSetBlob = procedure(obj: PUCMPlugin; key: PChar;
        Blob: UIntPtr; size: cardinal); cdecl;
    TUCMCB_StoreItemDel = procedure(obj: PUCMPlugin; key: PChar); cdecl;

    TUCMCB_MLMsgSend = function(id: cardinal; ctx: UIntPtr; x1: cardinal;
        x2: cardinal): integer; cdecl;
    TUCMCB_MLEventGetMem = function(id: integer): TUCMEvent; cdecl;
    TUCMCB_MLEventPush = function(event: TUCMEvent; x1: cardinal;
        x2: cardinal; Sender: Pointer): integer; cdecl;
    TUCMCB_MLEventFreeAndNil = procedure(event: PPUCMEvent); cdecl;
    TUCMCB_MLClear = procedure(); cdecl;

    TUCMMd5Buf = array [1..16] of byte;
    TUCMCB_Md5Buffer = procedure(buf: TUCMMd5Buf; inbuf: PChar; size: integer); cdecl;
    TUCMCB_Md5String = procedure(outbuf: PChar; const buf: TUCMMd5Buf); cdecl;

    TUCMCB_LogBase = procedure(plugin: TUCMPlugin; fmt: PChar); cdecl; varargs;
    TUCMCB_LogCore = procedure(fmt: PChar); cdecl; varargs;

    TUCMCB_Logger = procedure(Sender: PUCMPlugin; log_type: cardinal;
        txt: PChar); cdecl;
    TUCMCB_LogUnitConnect = procedure(callback: TUCMCB_Logger); cdecl;
    TUCMCB_LogUnitDisconnect = procedure(callback: TUCMCB_Logger); cdecl;

    TUCMCB_PluginsList = function(): PPUCMPlugin; cdecl;

    TUCMCB_PathGet = function(): PWideChar; cdecl;

    TUCMCB_PluginInfo = function(pid: PChar): PUCMPluginInfo; cdecl;

    TUCMCB_UMsgSend = function(): TUCMRetStatus; cdecl;
    TUCMCB_UMsgRecieve = function(): TUCMRetStatus; cdecl;

    TUCMCB_HookEvent = procedure(id: cardinal; ev: UIntPtr; x1: cardinal;
        x2: cardinal; ctx: Pointer); cdecl;
    TUCMCB_HookAttach = procedure(hook: TUCMCB_HookEvent; ctx: Pointer); cdecl;
    TUCMCB_HookDetach = procedure(hook: TUCMCB_HookEvent); cdecl;

    TUCMCB_RandEntropy = function(): integer; cdecl;

    TUCMFunctions = packed record
        malloc: TUCMCB_MemAlloc;
        zmalloc: TUCMCB_MemZeroAlloc;
        calloc: TUCMCB_MemCellAlloc;
        Free: TUCMCB_MemFree;
        zmemory: TUCMCB_MemZero;
        realloc: TUCMCB_MemRealloc;
        strdup: TUCMCB_StrDup;

        thread_create: TUCMCB_ThreadCreate;
        thread_detach: TUCMCB_ThreadOperation;
        thread_exit: TUCMCB_ThreadExit;
        thread_join: TUCMCB_ThreadOperation;

        mutex_create: TUCMCB_MutexCreate;
        mutex_free: TUCMCB_MutexFree;
        mutex_lock: TUCMCB_MutexLock;
        mutex_unlock: TUCMCB_MutexUnlock;

        cond_create: TUCMCB_CondCreate;
        cond_free: TUCMCB_CondFree;
        cond_wait: TUCMCB_CondOperation;
        cond_signal: TUCMCB_CondOperation;
        cond_broadcast: TUCMCB_CondOperation;

        rwlock_create: TUCMCB_MutexCreate;
        rwlock_free: TUCMCB_MutexFree;
        rwlock_rlock: TUCMCB_MutexLock;
        rwlock_wlock: TUCMCB_MutexLock;
        rwlock_unlock: TUCMCB_MutexUnlock;

        // TODO Unicode functions

        // low-level settings provider functions */
        get_int: TUCMCB_StoreGetInt;
        get_int64: TUCMCB_StoreGetLongInt;
        get_float: TUCMCB_StoreGetSingle;
        get_str: TUCMCB_StoreGetPChar;
        get_wstr: TUCMCB_StoreGetPWideChar;
        get_blob: TUCMCB_StoreGetBlob;
        set_int: TUCMCB_StoreSetInt;
        set_int64: TUCMCB_StoreSetLongInt;
        set_float: TUCMCB_StoreSetSingle;
        set_str: TUCMCB_StoreSetPChar;
        set_wstr: TUCMCB_StoreSetPWideChar;
        set_blob: TUCMCB_StoreSetBlob;
        item_del: TUCMCB_StoreItemDel;

        // general queue access */
        mainloop_msg_send: TUCMCB_MLMsgSend;
        mainloop_ev_alloc: TUCMCB_MLEventGetMem;
        mainloop_ev_push: TUCMCB_MLEventPush;
        mainloop_ev_free: TUCMCB_MLEventFreeAndNil;
        mainloop_flush: TUCMCB_MLClear;

        mainloop_hook_attach: TUCMCB_HookAttach;
        mainloop_hook_detach: TUCMCB_HookDetach;

        // get MD5 hash function */
        md5: TUCMCB_Md5Buffer;
        md5_to_str: TUCMCB_Md5String;

        // log and trace messages handlers*/
        log: TUCMCB_LogBase;
        ucm_log: TUCMCB_LogCore;
        logger_connect: TUCMCB_LogUnitConnect;
        logger_disconnect: TUCMCB_LogUnitDisconnect;

        // get plugins by category */
        get_plugins_all: TUCMCB_PluginsList;
        get_plugins_db: TUCMCB_PluginsList;
        get_plugins_proto: TUCMCB_PluginsList;
        get_plugins_crypt: TUCMCB_PluginsList;
        get_plugins_hist: TUCMCB_PluginsList;
        get_plugins_gui: TUCMCB_PluginsList;
        get_plugins_stuff: TUCMCB_PluginsList;

        // get global paths */
        get_startup_path: TUCMCB_PathGet;
        get_store_path: TUCMCB_PathGet;
        get_plugins_path: TUCMCB_PathGet;

        // system entropy functions
        get_entropy: TUCMCB_RandEntropy;

        // user API */
        get_plugin_info: TUCMCB_PluginInfo;
        ucm_send_message: TUCMCB_UMsgSend;
        ucm_recv_message: TUCMCB_UMsgRecieve;
    end;
    PUCMFunctions = ^TUCMFunctions;

    //TUCMCoreFlags=(UCM_FLAG_CORE_DBNEW=1,UCM_FLAG_CORE_DBRO=2);

    TUCMCoreArgs = packed record
        path_abs: PChar;
        path_plug_abs: PChar;
        path_store_abs: PChar;

        options: QWord;
    end;
    PUCMCoreArgs = ^TUCMCoreArgs;

{$IFDEF STATIC_LINK}
  {$IFDEF FPC}
    {$IFDEF WINDOWS}
        function UCMCoreStart(args: PUCMCoreArgs): PUCMFunctions;
            stdcall; external UCMConst_LibName Name 'ucm_core_start';
        function UCMCoreStop(): TUCMRetStatus; stdcall;
            external UCMConst_LibName Name 'ucm_core_stop';
        function UCMCoreInfo(): PUCMPluginInfo; stdcall;
            external UCMConst_LibName Name 'ucm_core_info';
    {$ELSE}
        function UCMCoreStart(args: PUCMCoreArgs): PUCMFunctions;
            cdecl; external UCMConst_LibName Name 'ucm_core_start';
        function UCMCoreStop(): TUCMRetStatus; cdecl;
            external UCMConst_LibName Name 'ucm_core_stop';
        function UCMCoreInfo(): PUCMPluginInfo; cdecl;
            external UCMConst_LibName Name 'ucm_core_info';
    {$ENDIF}
  {$ELSE}
       {TODO}
  {$ENDIF}
{$ELSE}
   {$IFDEF FPC}
     {$IFDEF WINDOWS}
       TUCMCB_CoreStart = function (args: PUCMCoreArgs): PUCMFunctions; stdcall;
       TUCMCB_CoreStop = function (): TUCMRetStatus; stdcall;
       TUCMCB_CoreInfo = function (): PUCMPluginInfo; stdcall;
     {$ELSE}
       TUCMCB_CoreStart = function (args: PUCMCoreArgs): PUCMFunctions; cdecl;
       TUCMCB_CoreStop = function (): TUCMRetStatus; cdecl;
       TUCMCB_CoreInfo = function (): PUCMPluginInfo; cdecl;
     {$ENDIF}
   {$ELSE}
          {TODO}
   {$ENDIF}
{$ENDIF}

implementation

end.
