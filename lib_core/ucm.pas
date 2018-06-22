unit ucm;

interface

const
{$IFDEF UNIX}
    clibname = 'libucm.so';
{$ELSE}
    clibname = 'libucm.dll';
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
    UCM_RET_BUSY
);

TUCMLogType = (
    UCM_LOG_INFO    = 1,
    UCM_LOG_DEBUG   = 2,
    UCM_LOG_ERROR   = 4
);

// ######################################################################
//  PLUGINS FUNCTIOANLITY
// ######################################################################

TUCMVAPI = record
    vmajor: byte;
    vminor: byte;
end;

TUCMPluginInfo = record
    api         : TUCMVAPI;
    sys         : byte;
    vmajor      : Word;
    vminor      : Word;
    vpatch      : Word;
    pid         : PChar;
    flags       : LongWord;
    build       : record
        commit      : PChar;
        datetime    : PChar;
        target      : PChar;
        compiler    : PChar;
        options     : PChar;
        flags       : PChar;
    end;
    name        : PChar;
    developer   : PChar;
    description : PChar;
    copyright   : PChar;
    email       : PChar;
    website     : PChar;
end;
PUCMPluginInfo = ^TUCMPluginInfo;

TUCMCB_PlugProc    = procedure (); cdecl;
TUCMCB_PlugFunc = function (): TUCMRetStatus; cdecl;
TUCMCB_PlugMessage   = procedure (id: Cardinal; ctx: UIntPtr; x1: Cardinal; x2: Cardinal); cdecl;

TUCMPlugin = record
    info    : TUCMPluginInfo;
    run     : TUCMCB_PlugFunc;
    stop    : TUCMCB_PlugFunc;
    message : TUCMCB_PlugMessage;
end;
PUCMPlugin = ^TUCMPlugin;

// ######################################################################
//      CHAT FUNCTIONALITY API IMPLEMENTATION
// ######################################################################

TUCMEvent = record
    ev      : Byte;
    size    : QWord;
    sender  : Pointer;
end;
PUCMEvent = ^TUCMEvent;
PPUCMEvent = ^PUCMEvent;

TUCMDatabase = record
    fname: TUCMPath;
    handler: PUCMPlugin;
end;
PUCMDatabase = ^TUCMDatabase;

TUCMCB_DbOpen       = function  (db: TUCMDatabase; ctx: Pointer): PUCMDatabase; cdecl;
TUCMCB_DbCheck      = function  (db: TUCMDatabase; ctx: Pointer): TUCMRetStatus; cdecl;
TUCMCB_DbFlush      = function  (db: TUCMDatabase): TUCMRetStatus; cdecl;
TUCMCB_DbClose      = function  (db: TUCMDatabase): TUCMRetStatus; cdecl;
TUCMCB_DbGetInt     = function  (db: TUCMDatabase; key: PChar; def: Integer): Integer; cdecl;
TUCMCB_DbGetLInt    = function  (db: TUCMDatabase; key: PChar; def: LongInt): LongInt; cdecl;
TUCMCB_DbGetSingle  = function  (db: TUCMDatabase; key: PChar; def: Single): Single; cdecl;
TUCMCB_DbGetPChar   = function  (db: TUCMDatabase; key: PChar; def: PChar): PChar; cdecl;
TUCMCB_DbSetInt     = procedure (db: TUCMDatabase; key: PChar; value: Integer); cdecl;
TUCMCB_DbSetLInt    = procedure (db: TUCMDatabase; key: PChar; value: LongInt); cdecl;
TUCMCB_DbSetSingle  = procedure (db: TUCMDatabase; key: PChar; value: Single); cdecl;
TUCMCB_DbSetPChar   = procedure (db: TUCMDatabase; key: PChar; value: PChar); cdecl;
TUCMCB_DbItemDel    = procedure (db: TUCMDatabase; key: PChar); cdecl;

TUCMDbPlugin = record
    core        : TUCMPlugin;
    db_open     : TUCMCB_DbOpen;
    db_check    : TUCMCB_DbCheck;
    db_flush    : TUCMCB_DbFlush;
    db_close    : TUCMCB_DbClose;

    get_int     : TUCMCB_DbGetInt;
    get_int64   : TUCMCB_DbGetLInt;
    get_float   : TUCMCB_DbGetSingle;
    get_str     : TUCMCB_DbGetPChar;
    set_int     : TUCMCB_DbSetInt;
    set_int64   : TUCMCB_DbSetLInt;
    set_float   : TUCMCB_DbSetSingle;
    set_str     : TUCMCB_DbSetPChar;
    item_del    : TUCMCB_DbItemDel;
end;
PUCMDbPlugin = ^TUCMDbPlugin;

// ######################################################################
//  API FUNCTIONALITY
// ######################################################################

TUCMCB_ThreadFunc       = procedure (ctx: Pointer); cdecl;
TUCMCB_ThreadCreate     = function (func: TUCMCB_ThreadFunc; ctx: Pointer): UIntPtr; cdecl;
TUCMCB_ThreadOperation  = function (tid: UIntPtr): Integer; cdecl;
TUCMCB_ThreadExit       = procedure (ret: Pointer); cdecl;

TUCMCB_MutexCreate  = function  (): UIntPtr; cdecl;
TUCMCB_MutexFree    = procedure (_mtx: UIntPtr); cdecl;
TUCMCB_MutexLock    = function  (_mtx: UIntPtr): Integer; cdecl;
TUCMCB_MutexUnlock  = function  (_mtx: UIntPtr): Integer; cdecl;

TUCMCB_CondCreate    = function  (): UIntPtr; cdecl;
TUCMCB_CondFree      = procedure (_cond: UIntPtr) cdecl;
TUCMCB_CondWait      = function  (_cond: UIntPtr; _mtx: UIntPtr): Integer; cdecl;
TUCMCB_CondSignal    = function  (_cond: UIntPtr): Integer; cdecl;
TUCMCB_CondBroadcast = function  (_cond: UIntPtr): Integer; cdecl;

TUCMCB_StoreGetInt       = function  (key: PChar; def: Integer): Integer; cdecl;
TUCMCB_StoreGetLongInt   = function  (key: PChar; def: LongInt): LongInt; cdecl;
TUCMCB_StoreGetSingle    = function  (key: PChar; def: Single): Single; cdecl;
TUCMCB_StoreGetPChar     = function  (key: PChar; def: PChar): PChar; cdecl;
TUCMCB_StoreGetPCharCopy = function  (key: PChar; def: PChar): PChar; cdecl;
TUCMCB_StoreSetInt       = procedure (key: PChar; value: Integer) ; cdecl;
TUCMCB_StoreSetLongInt   = procedure (key: PChar; value: LongInt) ; cdecl;
TUCMCB_StoreSetSingle    = procedure (key: PChar; value: Single) ; cdecl;
TUCMCB_StoreSetPChar     = procedure (key: PChar; value: PChar) ; cdecl;
TUCMCB_StoreItemDel      = procedure (key: PChar) ; cdecl;

TUCMCB_MLMsgSend         = function  (id: Cardinal; ctx: UIntPtr; x1: Cardinal; x2: Cardinal): Integer ; cdecl;
TUCMCB_MLEventGetMem     = function  (id: Integer): TUCMEvent; cdecl;
TUCMCB_MLEventPush       = function  (event: TUCMEvent; x1: Cardinal; x2: Cardinal; sender: Pointer): Integer; cdecl;
TUCMCB_MLEventFreeAndNil = procedure (event: PPUCMEvent); cdecl;
TUCMCB_MLClear           = procedure (); cdecl;

TUCMMd5Buf  =   array [1..16] of byte;
TUCMCB_Md5Buffer   = procedure (buf: TUCMMd5Buf; inbuf: PChar; size: Integer); cdecl;
TUCMCB_Md5String   = procedure (outbuf: PChar; const buf: TUCMMd5Buf); cdecl;

TUCMCB_LogBase             = procedure (plugin: TUCMPlugin; fmt: PChar); cdecl; varargs;
TUCMCB_LogCore             = procedure (fmt: PChar); cdecl; varargs;

TUCMCB_Logger              = procedure (sender: PUCMPlugin; log_type: Cardinal; txt: PChar); cdecl;
TUCMCB_LogUnitConnect      = procedure (callback: TUCMCB_Logger); cdecl;
TUCMCB_LogUnitDisconnect   = procedure (callback: TUCMCB_Logger); cdecl;

TUCMCB_PluginsList  = function(): PUCMPlugin; cdecl;

TUCMCB_PathGet  = function(): PChar; cdecl;

TUCMCB_PluginInfo = function (pid: PChar): PUCMPluginInfo; cdecl;

TUCMCB_UMsgSend    = function (): TUCMRetStatus; cdecl;
TUCMCB_UMsgRecieve = function (): TUCMRetStatus; cdecl;

TUCMFunctions = record
    thread_create       : TUCMCB_ThreadCreate;
    thread_detach       : TUCMCB_ThreadOperation;
    thread_exit         : TUCMCB_ThreadExit;
    thread_join         : TUCMCB_ThreadOperation;

    mutex_create        : TUCMCB_MutexCreate;
    mutex_free          : TUCMCB_MutexFree;
    mutex_lock          : TUCMCB_MutexLock;
    mutex_unlock        : TUCMCB_MutexUnlock;

    cond_create         : TUCMCB_CondCreate;
    cond_free           : TUCMCB_CondFree;
    cond_wait           : TUCMCB_CondWait;
    cond_signal         : TUCMCB_CondSignal;
    cond_broadcast      : TUCMCB_CondBroadcast;

    rwlock_create       : TUCMCB_MutexCreate;
    rwlock_free         : TUCMCB_MutexFree;
    rwlock_rlock        : TUCMCB_MutexLock;
    rwlock_wlock        : TUCMCB_MutexLock;
    rwlock_unlock       : TUCMCB_MutexUnlock;

        // low-level settings provider functions */
    get_int             : TUCMCB_StoreGetInt;
    get_int64           : TUCMCB_StoreGetLongInt;
    get_float           : TUCMCB_StoreGetSingle;
    get_str             : TUCMCB_StoreGetPChar;
    set_int             : TUCMCB_StoreSetInt;
    set_int64           : TUCMCB_StoreSetLongInt;
    set_float           : TUCMCB_StoreSetSingle;
    set_str             : TUCMCB_StoreSetPChar;
    item_del            : TUCMCB_StoreItemDel;

        // general queue access */
    mainloop_msg_send   : TUCMCB_MLMsgSend;
    mainloop_ev_alloc   : TUCMCB_MLEventGetMem;
    mainloop_ev_push    : TUCMCB_MLEventPush;
    mainloop_ev_free    : TUCMCB_MLEventFreeAndNil;
    mainloop_flush      : TUCMCB_MLClear;

        // get MD5 hash function */
    md5                 : TUCMCB_Md5Buffer;
    md5_to_str          : TUCMCB_Md5String;

        // log and trace messages handlers*/
    log                 : TUCMCB_LogBase;
    ucm_log             : TUCMCB_LogCore;
    logger_connect      : TUCMCB_LogUnitConnect;
    logger_disconnect   : TUCMCB_LogUnitDisconnect;

        // get plugins by category */
    get_plugins_all     : TUCMCB_PluginsList;
    get_plugins_db      : TUCMCB_PluginsList;
    get_plugins_net     : TUCMCB_PluginsList;
    get_plugins_crypt   : TUCMCB_PluginsList;
    get_plugins_hist    : TUCMCB_PluginsList;
    get_plugins_stuff   : TUCMCB_PluginsList;

        // get global paths */
    get_startup_path    : TUCMCB_PathGet;
    get_store_path      : TUCMCB_PathGet;
    get_plugins_path    : TUCMCB_PathGet;
    get_doc_path        : TUCMCB_PathGet;

        // user API */
    get_plugin_info     : TUCMCB_PluginInfo;
    ucm_send_message    : TUCMCB_UMsgSend;
    ucm_recv_message    : TUCMCB_UMsgRecieve;
end;
PUCMFunctions = ^TUCMFunctions;

{$IFDEF FPC}
function UCMCoreStart (const PathAbs: PChar; const UCMPathStoreAbs: PChar): PUCMFunctions; cdecl; external clibname name 'ucm_core_start';
function UCMCoreStop  (): TUCMRetStatus; cdecl; external clibname name 'ucm_core_stop';
function UCMCoreInfo  (): PUCMPluginInfo; cdecl; external clibname name 'ucm_core_info';
{$ELSE}
    {TODO}
{$ENDIF}
implementation
end.
