
{ Global compiler directives }
{$include bold.inc}
unit BoldLazarusRequirements;

{ What a project gets when a Bold component is dropped on a form in the
  Lazarus form designer (#117). The IDE adds the package of the component's
  unit, the runtime package, and also the design-time package that registered
  the component. A project must not require that one: it would link the
  registration code and the IDE's interface into the application.
  The registration unit registers, for each of its components, requirements
  (RegisterComponentRequirements) that take the design-time package out of
  the list again.

  In the Lazarus design-time package dclBold (packages/Lazarus/dclBold.lpk);
  dclBoldZeos uses it too. }

interface

uses
  Classes,
  ComponentEditors;

type
  { TBoldRuntimeOnlyRequirements }
  TBoldRuntimeOnlyRequirements = class(TComponentRequirements)
  public
    // The design-time package that is not required
    class function DesignPackageName: AnsiString; virtual; abstract;
    procedure RequiredPkgs(Pkgs: TStrings); override;
  end;

  { The components that dclBold registers }
  TBoldDclBoldNotRequired = class(TBoldRuntimeOnlyRequirements)
  public
    class function DesignPackageName: AnsiString; override;
  end;

implementation

uses
  SysUtils;

{ TBoldRuntimeOnlyRequirements }

procedure TBoldRuntimeOnlyRequirements.RequiredPkgs(Pkgs: TStrings);
var
  i: Integer;
begin
  for i := Pkgs.Count - 1 downto 0 do
    if SameText(Pkgs[i], DesignPackageName) then
      Pkgs.Delete(i);
end;

{ TBoldDclBoldNotRequired }

class function TBoldDclBoldNotRequired.DesignPackageName: AnsiString;
begin
  Result := 'dclBold';
end;

end.
