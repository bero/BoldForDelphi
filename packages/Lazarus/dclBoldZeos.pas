{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclBoldZeos;

{$warn 5023 off : no warning about unused units}
interface

uses
  BoldZeosLazarusReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BoldZeosLazarusReg', @BoldZeosLazarusReg.Register);
end;

initialization
  RegisterPackage('dclBoldZeos', @Register);
end.
