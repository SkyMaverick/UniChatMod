program UniChatMod;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    uMainForm,
    lazrichview,
    uAboutForm,
    uConfForm,
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
        UCMCore := TUCMCore.Create;
        UCMCore.Initialize(ProgramDirectory, '');
        Application.CreateForm(TfmMain, fmMain);
        Application.Run;
    finally
        if (UCMCore <> nil) then
        begin
             UCMCore.Finish();
             UCMCore.Free;
        end;
    end;
end.
