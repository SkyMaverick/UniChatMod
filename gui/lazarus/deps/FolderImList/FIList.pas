unit FIList;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FileUtil;

type

    { TFolderImageList }

    TFolderImageList = class(TImageList)
    private
        ImageNames: TStringList;
        AILPath: string;
        DPIScale: boolean;
        IconsExtMask: string;

        procedure ResizeWithDPI;
        function GetLoaded: boolean;
        procedure CreateTargetPath(Folder: string; SubFolder: string);
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        function LoadFolder(Folder: string): cardinal; overload;
        function LoadFolder(Folder: string; SubFolder: string): cardinal; overload;
        function GetIndexByName(ImageName: string): integer;

        property Loaded: boolean read GetLoaded;
        property Path: string read AILPath;
    published
        property UseDPISupport: boolean read DPIScale write DPIScale;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Common Controls', [TFolderImageList]);
end;

{ TFolderImageList }

procedure TFolderImageList.ResizeWithDPI;
var
    Res: cardinal;
begin
    Res := Forms.Screen.PixelsPerInch;
    with Self do
    begin
        if Res >= 120 then
            if Res >= 144 then
                if Res >= 192 then
                begin
                    Height := Height * 2;
                    Width := Width * 2;
                end
                else
                begin
                    Height := round(Height * 1.5);
                    Width := round(Width * 1.5);
                end
            else
            begin
                Height := round(Height * 1.25);
                Width := round(Width * 1.25);
            end;
    end;
end;

function TFolderImageList.GetLoaded: boolean;
begin
    Result := False;
    if ImageNames <> nil then
        Result := True;
end;

procedure TFolderImageList.CreateTargetPath(Folder: string; SubFolder: string);
var
    ATmp: string;
begin
    if UseDPISupport = True then
        ATmp := Folder + IntToStr(Self.Width) + 'x' + IntToStr(Self.Height) + PathDelim
    else
        ATmp := Folder;

    if not SubFolder.IsEmpty then
        ATmp := ATmp + SubFolder + PathDelim;
    if DirectoryExists(ATmp) then
        AILPath := ATmp
    else
        raise EFileNotFoundException.CreateFmt('Path not found: %s', [ATmp]);
end;

constructor TFolderImageList.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    ImageNames := TStringList.Create;
    if ImageNames = nil then
        raise EAccessViolation.Create('Image list names DONT create');
    if UseDPISupport = True then
        ResizeWithDPI;
end;

destructor TFolderImageList.Destroy;
begin
    if ImageNames <> nil then
        FreeAndNil(ImageNames);
    inherited Destroy;
end;

function TFolderImageList.LoadFolder(Folder: string): cardinal;
begin
    Result := LoadFolder(Folder, '');
end;

function TFolderImageList.LoadFolder(Folder: string; SubFolder: string): cardinal;
var
    AFiles: TStringList;
    png: TPortableNetworkGraphic;
    FName: string;
    i: cardinal;
begin
    AFiles := TStringList.Create;
    try
        CreateTargetPath(Folder, SubFolder);
        if not Path.IsEmpty then
            FindAllFiles(AFiles, Path, '*.png', False, faAnyFile);
        if AFiles.Count > 0 then
        begin
            png := TPortableNetworkGraphic.Create;
            try
                for i := 0 to AFiles.Count - 1 do
                begin
                    png.LoadFromFile(AFiles.Strings[i]);
                    Self.Add(png, nil);
                    ImageNames.Add(ExtractFileNameWithoutExt(
                        ExtractFileName(AFiles.Strings[i])));
                end;
            finally
                png.Free;
            end;
        end;
    finally
        AFiles.Free;
    end;
    Result := Self.Count;
end;

function TFolderImageList.GetIndexByName(ImageName: string): integer;
begin
    Result := -1;
    if not ImageName.IsEmpty then
        Result := ImageNames.IndexOf(ImageName);
end;

initialization
    {$I folderimagelist.lrs }

end.
