unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ExtCtrls, StdCtrls, ComCtrls, ComboEx, RichView, RVStyle;

type

  { TfmMain }

  TfmMain = class(TForm)
      alActions: TActionList;
      cbUserChange: TComboBoxEx;
      cbTopicChange: TComboBoxEx;
      ilBtnIcons: TImageList;
      ListView1: TListView;
      memInput: TMemo;
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
      rvReports: TRichView;
      rvHist: TRichView;
      rvsArea: TRVStyle;
      Splitter1: TSplitter;
      Splitter2: TSplitter;
      tsReports: TTabSheet;
      tiIcon: TTrayIcon;
      tbInputCtrls: TToolBar;
      tbUsersCtrl: TToolBar;
      ToolButton1: TToolButton;
      ToolButton2: TToolButton;
      ToolButton3: TToolButton;
      ToolButton4: TToolButton;
      tsMain: TTabSheet;
  private

  public

  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

end.

