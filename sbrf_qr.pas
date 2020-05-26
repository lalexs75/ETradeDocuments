{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sbrf_qr;

{$warn 5023 off : no warning about unused units}
interface

uses
  lrSBRF_QR, lrSBRF_QR_Editor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lrSBRF_QR', @lrSBRF_QR.Register);
end;

initialization
  RegisterPackage('sbrf_qr', @Register);
end.
