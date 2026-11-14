{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclBold;

{$warn 5023 off : no warning about unused units}
interface

uses
  BoldLazarusReg, BoldOclExpressionPropEdit, 
  BoldIDEConsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BoldLazarusReg', @BoldLazarusReg.Register);
end;

initialization
  RegisterPackage('dclBold', @Register);
end.
