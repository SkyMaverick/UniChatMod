unit uCoreClass;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dynlibs, ucm;

type

    { TUCMCore }
    EUCMCoreException = class(Exception);
    EUCMCoreCreateFail = class(EUCMCoreException);
    EUCMCoreAPIFail = class(EUCMCoreException);
    EUCMCoreInfoFail = class(EUCMCoreException);

    TUCMCore = class(TObject)
    private

        Refs: cardinal;
        CoreAPI: PUCMFunctions;
        InfoPtr: PUCMPluginInfo;

        CoreHandle: TLibHandle;
        CoreStartCB: TUCMCB_CoreStart;
        CoreStopCB: TUCMCB_CoreStop;
        CoreInfoCB: TUCMCB_CoreInfo;

        function GetInfo: TUCMPluginInfo;
        function GetCorePath: string;
        function GetStorePath: string;
        function GetModsPath: string;
        function GetDocsPath: string;
    protected
    public
        // make this class as TObject-child singleton
        class function NewInstance: TObject; override;
        procedure FreeInstance; override;
        property InstanceCount: cardinal read Refs;

        procedure Initialize(const PathCoreAbs: string; const PathStoreAbs: string);
        procedure Finish();

        //        procedure AttachHook
        property Info: TUCMPluginInfo read GetInfo;
        property PathCore: string read GetCorePath;
        property PathStore: string read GetStorePath;
        property PathModules: string read GetModsPath;
        property PathDocuments: string read GetDocsPath;
    end;

implementation

{ TUCMCore }

var
    __UCMCoreSingleton: TUCMCore;

function TUCMCore.GetInfo: TUCMPluginInfo;
begin
    Result := InfoPtr^;
end;

function TUCMCore.GetCorePath: string;
begin
    Result := ansistring(CoreAPI^.get_startup_path());
end;

function TUCMCore.GetStorePath: string;
begin
    Result := ansistring(CoreAPI^.get_store_path());
end;

function TUCMCore.GetModsPath: string;
begin
    Result := ansistring(CoreAPI^.get_plugins_path());
end;

function TUCMCore.GetDocsPath: string;
begin
    Result := StrPas(CoreAPI^.get_doc_path());
end;

class function TUCMCore.NewInstance: TObject;
begin
    if __UCMCoreSingleton = nil then
        __UCMCoreSingleton := TUCMCore(inherited NewInstance);
    Result := __UCMCoreSingleton;
    __UCMCoreSingleton.Refs := __UCMCoreSingleton.Refs + 1;
end;

procedure TUCMCore.FreeInstance;
begin
    __UCMCoreSingleton.Refs := __UCMCoreSingleton.Refs - 1;
    if (__UCMCoreSingleton.Refs = 0) then
    begin
        inherited FreeInstance;
        __UCMCoreSingleton := nil;
    end;
end;

procedure TUCMCore.Initialize(const PathCoreAbs: string; const PathStoreAbs: string);

    procedure Terminate(Ex: Exception);
    begin
        raise Ex;
        Finish();
        exit();
    end;

var
    LibPath: string;
begin
    {$IFDEF BUILD_BUNDLE}
        {$IFDEF UNIX}
    LibPath := PathCoreAbs + PathDelim + UCMConst_LibName;
        {$ENDIF}
    {$ELSE}
    LibPath := UCMConst_LibName;
    {$ENDIF}
    // Dynamic load library
    CoreHandle := LoadLibrary(LibPath);
    if CoreHandle = dynlibs.NilHandle then
    begin
        Terminate(EUCMCoreCreateFail.Create('Core library not loaded'));
    end;
    // Get API functions
    CoreStartCB := TUCMCB_CoreStart(GetProcedureAddress(CoreHandle, UCMStartProcName));
    CoreStopCB := TUCMCB_CoreStop(GetProcedureAddress(CoreHandle, UCMStopProcName));
    CoreInfoCB := TUCMCB_CoreInfo(GetProcedureAddress(CoreHandle, UCMInfoProcName));

    if (CoreStartCB = nil) or (CoreStopCB = nil) or (CoreInfoCB = nil) then
    begin
        Terminate(EUCMCoreAPIFail.Create('Core API get missing'));
    end;
    // Start core. Get CoreAPI or terminate
    CoreAPI := CoreStartCB(PChar(PathCoreAbs), PChar(PathStoreAbs));
    if (CoreAPI = nil) then
    begin
        Terminate(EUCMCoreAPIFail.Create('Core information get missing'));
    end;
    // Get core info or terminate
    InfoPtr := CoreInfoCB();
    if (InfoPtr = nil) then
    begin
        Terminate(EUCMCoreInfoFail.Create('Core information get missing'));
    end;
end;

procedure TUCMCore.Finish();
begin
    if (CoreHandle <> dynlibs.NilHandle) then
    begin
        if (CoreAPI <> nil) then
            CoreStopCB();
        FreeLibrary(CoreHandle);
    end;
end;

initialization

finalization

end.
