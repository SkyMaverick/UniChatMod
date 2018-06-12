unit uCoreClass;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ucm;

type

    { TUCMCore }
    EUCMCoreException = class(Exception);
    EUCMCoreCreateFail = class(EUCMCoreException);
    EUCMCoreAPIFail = class(EUCMCoreException);
    EUCMCoreInfoFail = class(EUCMCoreException);

    TUCMCore = class(TObject)
    private
        CoreAPI: PUCMFunctions;
        InfoPtr: PUCMPluginInfo;
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

        procedure Initialize(const PathCoreAbs: string; const PathStoreAbs: string);
        procedure Finish();

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
end;

procedure TUCMCore.FreeInstance;
begin
    inherited FreeInstance;
    __UCMCoreSingleton := nil;
end;

procedure TUCMCore.Initialize(const PathCoreAbs: string; const PathStoreAbs: string);
begin
    //TODO
    CoreAPI := UCMCoreStart(PChar(PathCoreAbs), PChar(PathStoreAbs));
    if (CoreAPI = nil) then
    begin
        raise EUCMCoreAPIFail.Create('Core API get missing');
    end;
    InfoPtr := UCMCoreInfo();
    if (InfoPtr = nil) then
    begin
        raise EUCMCoreInfoFail.Create('Core information get missing');
    end;
end;

procedure TUCMCore.Finish();
begin
    if (CoreAPI <> nil) then
        UCMCoreStop();
end;

initialization

finalization

end.
