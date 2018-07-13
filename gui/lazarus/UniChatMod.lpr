program UniChatMod;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    sysutils,
    Forms, Interfaces,
    uMainForm,
    FileUtil,
    uCoreClass { you can add units after this };

{$R *.res}

var
    UCMCore: TUCMCore;

begin
    Application.Scaled := True;
    RequireDerivedFormResource := True;
    Application.Initialize;
    // Initialize core library
    try
        try
            UCMCore := TUCMCore.Create;
            UCMCore.Initialize(ProgramDirectory, '');
            Application.CreateForm(TfmMain, fmMain);
            Application.Run;
        except
            on E: Exception do
               Application.MessageBox(PChar(E.Message),PChar('Unhandled core exception'), 0);
        end;
    finally
        if (UCMCore <> nil) then
        begin
            UCMCore.Finish();
            UCMCore.Free;
        end;
    end;
end.
