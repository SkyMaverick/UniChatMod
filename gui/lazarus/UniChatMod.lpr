program UniChatMod;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm, ucoreclass, lazrichview
  { you can add units after this };

{$R *.res}

var
  UCMCore: TUCMCore;

begin
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  // Initialize core library
  try
     UCMCore := TUCMCore.Create;
  except
    Application.Terminate;
  end;
  if (UCMCore <> nil) then
  begin
       UCMCore.Initialize(ParamStr(0), '');
       Application.CreateForm(TfmMain, fmMain);
       Application.Run;
       UCMCore.Free;
  end;

end.

