unit uImageListHelper;

{$mode objfpc}{$H+}
interface

uses
    Classes, SysUtils, Controls, FileUtil, Graphics;

type

    { TImageListLoader }

    TImageListLoader = class helper for TImageList
        function LoadPNGFromPath(path: string): cardinal;
    end;

implementation

{ TImageListLoader }

function TImageListLoader.LoadPNGFromPath(path: string): cardinal;
var
    png: TPortableNetworkGraphic;
    AFiles: TStringList;
    i: cardinal;
begin
    AFiles := TStringList.Create;
    try
        FindAllFiles(AFiles, path, '*.png', False);
        if AFiles.Count > 0 then
        begin
            png := TPortableNetworkGraphic.Create;
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
    Result:=Self.Count;
end;

end.
