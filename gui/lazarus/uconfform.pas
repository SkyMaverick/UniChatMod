unit uConfForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
    ExtCtrls, StdCtrls;

type

    { TfmSettings }

    TfmSettings = class(TForm)
        btCancel: TButton;
        btOk: TButton;
        pcSettings: TPageControl;
        pnlButtons: TPanel;
        tvCategory: TTreeView;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private

    public

    end;

var
    fmSettings: TfmSettings;

implementation

{$R *.lfm}

{ TfmSettings }

procedure TfmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

end.
