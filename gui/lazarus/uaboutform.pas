unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls, ComCtrls;

type

    { TfmAbout }

    TfmAbout = class(TForm)
        imLogo: TImage;
        laStore: TLabel;
        laVersonU: TLabel;
        laVersionCoreU: TLabel;
        laBuildInfoU: TLabel;
        laFlagsU: TLabel;
        laStoreU: TLabel;
        laFlags: TLabel;
        laBuildInfo: TLabel;
        laOAuthorsList: TLabel;
        laOrigAuthors: TLabel;
        laVersionCore: TLabel;
        laVersion: TLabel;
        laProject: TLabel;
        memAuthors: TMemo;
        memLicense: TMemo;
        memModules: TMemo;
        memTranslates: TMemo;
        tbsTranslates: TTabSheet;
        tbsLibs: TTabSheet;
        tbsLicense: TTabSheet;
        tbInfo: TPageControl;
        pnlOrigAuthors: TPanel;
        pnlTop: TPanel;
        tbsAuthors: TTabSheet;
    private

    public

    end;

var
    fmAbout: TfmAbout;

implementation

{$R *.lfm}

end.

