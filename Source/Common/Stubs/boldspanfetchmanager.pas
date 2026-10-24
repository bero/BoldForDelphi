{ Global compiler directives }
{$include bold.inc}
unit BoldSpanFetchManager;

{
  BoldSpanFetchManager - Wrapper unit for AttracsSpanFetchManager compatibility
  See BoldStubs.pas for documentation on stub strategy.

  Strategy: RETURN SAFE DEFAULT / DO NOTHING
  InSpanFetch returns False, PrefetchDerivedMember does nothing.
  This safely disables SpanFetch optimization when real Attracs units unavailable.
}

interface

uses
  BoldElements;

function InSpanFetch: Boolean;
procedure PrefetchDerivedMember(Element: TBoldElement);

implementation

uses
  BoldStubs;

function InSpanFetch: Boolean;
begin
  Result := BoldStubs.InSpanFetch;
end;

procedure PrefetchDerivedMember(Element: TBoldElement);
begin
  BoldStubs.PrefetchDerivedMember(Element);
end;

end.
