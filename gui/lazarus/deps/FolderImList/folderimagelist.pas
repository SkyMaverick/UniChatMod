{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit folderimagelist;

{$warn 5023 off : no warning about unused units}
interface

uses
  FIList, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FIList', @FIList.Register);
end;

initialization
  RegisterPackage('folderimagelist', @Register);
end.
