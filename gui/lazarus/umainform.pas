unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
    Menus, ExtCtrls, StdCtrls, ComCtrls, ComboEx, RichView, RVStyle,
    uAboutForm, uConfForm, uImageListHelper, uCoreClass;

type

    { TfmMain }

    TfmMain = class(TForm)
        actAreaClear: TAction;
        actAreaTxtCopy: TAction;
        actAreaAutoScroll: TAction;
        actDummy: TAction;
        actHlpAbout: TAction;
        actHlpHelp: TAction;
        actHlpSite: TAction;
        actHlpLicense: TAction;
        actHlpIssues: TAction;
        actMgmExit: TAction;
        actMgmArchive: TAction;
        actMgmMinimize: TAction;
        actMgmSettings: TAction;
        actUStatAbuse: TAction;
        actUStatNotConn: TAction;
        actUStatBusy: TAction;
        actUStatGame: TAction;
        actUStatActive: TAction;
        actMgmExitChat: TAction;
        actMgmChangeTheme: TAction;
        actUsersRefresh: TAction;
        actMulticast: TAction;
        alActions: TActionList;
        cbUserChange: TComboBoxEx;
        cbTopicChange: TComboBoxEx;
        ilBtnIcons: TImageList;
        ListView1: TListView;
        memInput: TMemo;
        MenuItem1: TMenuItem;
        MenuItem10: TMenuItem;
        MenuItem11: TMenuItem;
        MenuItem12: TMenuItem;
        MenuItem13: TMenuItem;
        MenuItem14: TMenuItem;
        MenuItem15: TMenuItem;
        MenuItem16: TMenuItem;
        MenuItem17: TMenuItem;
        MenuItem18: TMenuItem;
        MenuItem19: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem20: TMenuItem;
        MenuItem21: TMenuItem;
        MenuItem22: TMenuItem;
        MenuItem23: TMenuItem;
        MenuItem24: TMenuItem;
        MenuItem25: TMenuItem;
        MenuItem26: TMenuItem;
        MenuItem27: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        MenuItem8: TMenuItem;
        MenuItem9: TMenuItem;
        pmiCopyAll: TMenuItem;
        pmiClearArea: TMenuItem;
        pmiUsersMulticast: TMenuItem;
        pmiUsersRefresh: TMenuItem;
        mmiHelp: TMenuItem;
        mmiSettings: TMenuItem;
        mmiEdit: TMenuItem;
        mmiView: TMenuItem;
        mmiTalks: TMenuItem;
        mmMain: TMainMenu;
        pcTabs: TPageControl;
        pnlInput: TPanel;
        pnlWArea: TPanel;
        pnlWnd: TPanel;
        pnlUsers: TPanel;
        pmArea: TPopupMenu;
        pmUsers: TPopupMenu;
        pmTray: TPopupMenu;
        rvReports: TRichView;
        rvHist: TRichView;
        rvsArea: TRVStyle;
        vSplit: TSplitter;
        hSplit: TSplitter;
        tsReports: TTabSheet;
        tiIcon: TTrayIcon;
        tbInputCtrls: TToolBar;
        tbUsersCtrl: TToolBar;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        tsMain: TTabSheet;
        procedure actHlpAboutExecute(Sender: TObject);
        procedure actMgmSettingsExecute(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
    private
        UCMCore: TUCMCore;
    public

    end;

var
    fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.actHlpAboutExecute(Sender: TObject);
begin
    fmAbout := TfmAbout.Create(Self);
    actHlpAbout.Enabled := False;
    fmAbout.ShowModal;
    fmAbout.Free;
    actHlpAbout.Enabled := True;
end;

procedure TfmMain.actMgmSettingsExecute(Sender: TObject);
begin
    fmSettings := TfmSettings.Create(Self);
    actMgmSettings.Enabled := False;
    fmSettings.ShowModal;
    fmSettings.Free;
    actMgmSettings.Enabled := True;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    UCMCore.Free;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
    UCMCore:=TUCMCore.Create;
    ilBtnIcons.LoadPNGFromPath(ProgramDirectory+'icons'+PathDelim+'16x16');
end;

end.
