unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Forms, ExtCtrls, StdCtrls, ComCtrls, uCoreClass, fileinfo;

type

    { TfmAbout }

    TfmAbout = class(TForm)
        imLogo: TImage;
        laStore: TLabel;
        laVersionU: TLabel;
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
        memTranslates: TMemo;
        tbsTranslates: TTabSheet;
        tbsLicense: TTabSheet;
        tbInfo: TPageControl;
        pnlOrigAuthors: TPanel;
        pnlTop: TPanel;
        tbsAuthors: TTabSheet;
        procedure FormCreate(Sender: TObject);
        procedure tbsAuthorsShow(Sender: TObject);
        procedure tbsLicenseShow(Sender: TObject);
        procedure tbsTranslatesShow(Sender: TObject);
    private
        UCMCore: TUCMCore;
        isLoadedAuthors: boolean;
        isLoadedLicense: boolean;
        isLoadedTranslate: boolean;
    public

    end;

var
    fmAbout: TfmAbout;

implementation

{$R *.lfm}

{ TfmAbout }

function GetAppVersion(): string;
var
    AppVersion: TFileVersionInfo;
begin
    AppVersion := TFileVersionInfo.Create(nil);
    try
        AppVersion.ReadFileInfo;
        Result := AppVersion.VersionStrings.Values['ProductVersion'];
    finally
        AppVersion.Free;
    end;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
    UCMCore := TUCMCore.Create;
    with UCMCore do
    begin
        laVersionU.Caption := GetAppVersion();
        laVersionCoreU.Caption :=
            Format('%d.%d.%d', [Info.vmajor, Info.vminor, Info.vpatch]);
        laBuildInfoU.Caption := StrPas(Info.build.compiler);
        laFlagsU.Caption := StrPas(Info.build.options);
    end;
    isLoadedAuthors := False;
    isLoadedLicense := False;
    isLoadedTranslate := False;
end;

procedure TfmAbout.tbsAuthorsShow(Sender: TObject);
var
    fpath: string;
begin
    with memAuthors do
    begin
        if not isLoadedAuthors then
        begin
            Lines.Clear;
            fpath := UCMCore.PathDocuments + PathDelim + 'AUTHORS';
            if (FileExists(fpath)) then
            begin
                Lines.LoadFromFile(fpath);
                isLoadedAuthors := True;
            end
            else
                Lines.Add(Format('%s: %s', ['Not found file', fpath]));
        end;
    end;
end;

procedure TfmAbout.tbsLicenseShow(Sender: TObject);
var
    fpath: string;
begin
    with memLicense do
    begin
        if not isLoadedLicense then
        begin
            Lines.Clear;
            fpath := UCMCore.PathDocuments + PathDelim + 'LICENSE';
            if (FileExists(fpath)) then
            begin
                Lines.LoadFromFile(fpath);
                isLoadedLicense := True;
            end
            else
                Lines.Add(Format('%s: %s', ['Not found file', fpath]));
        end;
    end;
end;

procedure TfmAbout.tbsTranslatesShow(Sender: TObject);
var
    fpath: string;
begin
    with memTranslates do
    begin
        if not isLoadedTranslate then
        begin
            Lines.Clear;
            fpath := UCMCore.PathCore + PathDelim + 'TRANSLATE';
            if (FileExists(fpath)) then
            begin
                Lines.LoadFromFile(fpath);
                isLoadedLicense := True;
            end
            else
                Lines.Add(Format('%s: %s', ['Not found file', fpath]));
        end;
    end;
end;

end.


