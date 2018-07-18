unit uImageListHelper;

{$mode objfpc}{$H+}
interface

uses
    Classes, SysUtils, Controls, FileUtil, Graphics, Forms;

type

    { TImageListLoader }

    TImageListLoader = class helper for TImageList
        function LoadPNGFromPath(path: string): cardinal;
    end;

implementation

{ TImageListLoader }

function GetOptimalIconsSize(): cardinal;
begin
    Result := Forms.Screen.PixelsPerInch;
end;

function TImageListLoader.LoadPNGFromPath(path: string): cardinal;
var
    png: TPortableNetworkGraphic;
    AFiles: TStringList;
    i: cardinal;
begin
//    Application.MessageBox(PChar(IntToStr(GetOptimalIconsSize())), nil, 0);
    AFiles := TStringList.Create;
    try
        FindAllFiles(AFiles, path, '*.png', False);
        if AFiles.Count > 0 then
        begin
            png := TPortableNetworkGraphic.Create;
            if png = nil then
                raise EInvalidPointer.Create('Not memory allocate for icons');
            try
                for i := 0 to AFiles.Count - 1 do
                begin
                    png.LoadFromFile(AFiles.Strings[i]);
                    Self.Add(png, nil);
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

end.
