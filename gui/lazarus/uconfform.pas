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
    private

    public

    end;

var
    fmSettings: TfmSettings;

implementation

{$R *.lfm}

end.

