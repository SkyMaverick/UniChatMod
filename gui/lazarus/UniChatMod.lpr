program UniChatMod;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    uMainForm,
    ucoreclass,
    lazrichview,
    uAboutForm, uConfForm { you can add units after this };

{$R *.res}

var
    UCMCore: TUCMCore;

begin
    Application.Scaled := True;
    RequireDerivedFormResource := True;
    Application.Initialize;
    // Initialize core library
    UCMCore := TUCMCore.Create;
    try
        UCMCore.Initialize(ParamStr(0), '');
        Application.CreateForm(TfmMain, fmMain);
        Application.Run;
    finally
        UCMCore.Finish();
        UCMCore.Free;
    end;
end.
