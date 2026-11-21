# TBAFloat

`TBAFloat` (unit `BoldAttributes`) stores `Double` (64-bit IEEE floating-point) attribute values. The model types Float and Double map to it. Use it for numeric data where exact decimal precision is not critical.

## Class Hierarchy

```mermaid
classDiagram
    class TBANumeric {
        <<abstract>>
        +AsFloat: Double
        +IsNullOrZero()
    }
    TBANumeric <|-- TBAFloat
    TBANumeric <|-- TBACurrency
    TBANumeric <|-- TBAInteger
    TBANumeric <|-- TBAMoment

    class TBAFloat {
        +AsFloat Double
        +CreateWithValue()
    }

    click TBACurrency href "../TBACurrency/" "TBACurrency documentation"
    click TBAInteger href "../TBAInteger/" "TBAInteger documentation"
    click TBAMoment href "../TBAMoment/" "TBAMoment documentation"
```

## Class Definition

```pascal
TBAFloat = class(TBANumeric)
public
  constructor CreateWithValue(Value: Double);
  function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
  function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
  procedure SetEmptyValue; override;
  procedure Assign(Source: TBoldElement); override;
  function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
  function CanSetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean;
  property AsFloat: Double read GetAsFloat write SetAsFloat;
  property AsInteger: Integer write SetAsInteger;
end;
```

## Working with TBAFloat

### Reading and Writing

```pascal
// Direct property access
Price := Product.UnitPrice;
Product.UnitPrice := 29.99;

// Raw member access
Product.M_UnitPrice.AsFloat := 29.99;
```

### Null and Zero Checking

```pascal
if Product.M_UnitPrice.IsNull then
  ShowMessage('Price not set');

if Product.M_UnitPrice.IsNullOrZero then
  ShowMessage('No price');
```

### OCL Operations

The OCL engine supports several operations on float values:

```
self.unitPrice.formatFloat('#,##0.00')   // Format for display
self.unitPrice.simpleRound               // Round to integer (the value is a Float)
self.unitPrice.simpleRoundTo(-2)         // Math.SimpleRoundTo; -2 rounds to 2 decimals (the value is a Float)
self.total.safediv(self.quantity)        // Division that does not raise when the divisor is zero
```

Read the result of `simpleRound` with `EvaluateExpressionAsFloat`, not `EvaluateExpressionAsInteger`.

!!! warning "Locale Sensitivity"
    `StrToFloat` is locale-sensitive. In tests, use `FloatToStr()` to generate portable string values rather than hardcoding decimal separators.

## Common Patterns

### Formatting for Display

```pascal
function FormatPrice(Attr: TBAFloat): string;
begin
  if Attr.IsNull then
    Result := '-'
  else
    Result := FormatFloat('#,##0.00', Attr.AsFloat);
end;
```

### DUnitX Type Matching

```pascal
// Assert.AreEqual requires exact types — use Double() cast
Assert.AreEqual(Double(29.99), Product.M_UnitPrice.AsFloat);
```

## See Also

- [TBAFloat reference](../reference/BoldAttributes/TBAFloat.md) - Generated API reference
- [TBACurrency](TBACurrency.md) - Fixed-point alternative for financial data
- [TBAInteger](TBAInteger.md) - Integer sibling
- [TBoldAttribute](TBoldAttribute.md) - Base class
