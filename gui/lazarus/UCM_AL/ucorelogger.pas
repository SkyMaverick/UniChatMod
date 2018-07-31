unit uCoreLogger;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, uCoreClass, ucm;

type

    { TUCMCoreLogger }

    TFunctionLink = function(PluginID: string; MsgType: cardinal;
        Text: string; Args: Pointer): cardinal;

    TUCMCoreLogger = class(TObject)
    private
        Core: TUCMCore;

        LinkFunction: TFunctionLink;
        LinkArgs: Pointer;

        procedure Inject(Sender: PUCMPlugin; log_type: cardinal; txt: PChar); cdecl;
    public
        constructor Create(Owner: TUCMCore);
        procedure Link(Callback: TFunctionLink; Args: Pointer);
        destructor Destroy; override;
    end;

implementation

{ TUCMCoreLogger }

procedure TUCMCoreLogger.Inject(Sender: PUCMPlugin; log_type: cardinal;
    txt: PChar); cdecl;
begin
    Self := LinkFunction(UTF8String(Sender^.info.pid), log_type,
        UTF8String(txt), Self.LinkArgs);
end;

constructor TUCMCoreLogger.Create(Owner: TUCMCore);
begin
    if (Owner <> nil) and (Owner.InstanceCount > 0) then
    begin
        inherited Create;
        Core := TUCMCore.Create;
    end
    else
        raise EUCMCoreException.Create(
            'Create UCMCore instance before create logger');
end;

procedure TUCMCoreLogger.Link(Callback: TFunctionLink; Args: Pointer);
begin
    if Callback = nil then
        raise EArgumentException.Create('Callback must be function');
end;

destructor TUCMCoreLogger.Destroy;
begin
    Core.Free;
    inherited Destroy;
end;

end.
