unit ucm;

interface

type
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

TUCMPluginInfo = record
end;

PUCMPluginInfo = ^TUCMPluginInfo;

TUCMFunctions = record
end;

PUCMFunctions = ^TUCMFunctions;

{$IFDEF FPC}
function UCMCoreStart (const PathAbs: PChar; const UCMPathStoreAbs: PChar): PUCMFunctions; stdcall; external name 'ucm_core_start';
function UCMCoreStop  (): TUCMRetStatus; stdcall; external name 'ucm_core_stop';
function UCMCoreInfo  (): PUCMPluginInfo; stdcall; external name 'ucm_core_info';
{$ELSE}
    {TODO}
{$ENDIF}
implementation
end.
