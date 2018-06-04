unit uCoreClass;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ucm;

type

    { TUCMCore }

    TUCMCore = class(TObject)
    private
        CoreAPI: PUCMFunctions;
    protected
    public
        // make this class as TObject-child singleton
        class function NewInstance: TObject; override;
        procedure BeforeDestruction; override;

        procedure Initialize(const PathCoreAbs: string; const PathStoreAbs: string);
        procedure Finish();
    end;

implementation

{ TUCMCore }

var
    __UCMCoreSingleton: TUCMCore;

class function TUCMCore.NewInstance: TObject;
begin
    Result := __UCMCoreSingleton;
    if Result = nil then
    begin
        Result := inherited NewInstance;
        TUCMCore(__UCMCoreSingleton).Create;
    end;
end;

procedure TUCMCore.BeforeDestruction;
begin
    inherited BeforeDestruction;
end;

procedure TUCMCore.Initialize(const PathCoreAbs: string; const PathStoreAbs: string);
begin
    //TODO
    CoreAPI := UCMCoreStart(PChar(PathCoreAbs), PChar(PathStoreAbs));
end;

procedure TUCMCore.Finish();
begin
    //TODO
    UCMCoreStop();
end;

initialization

finalization

end.
