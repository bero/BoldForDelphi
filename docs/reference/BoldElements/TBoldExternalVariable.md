# TBoldExternalVariable

External variables are used to send additional values to an OCL-expression

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldExternalVariable = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldExternalVariable
4. **Direct subclasses**
5. `TBoldHandleBasedExternalVariable`

## Description

This is an abstract class and needs to be subclassed. The two operations that must be overridden are [GetValue](TBoldExternalVariable.md#getvalue) and [GetValueType](TBoldExternalVariable.md#getvaluetype)

Once an external variable is created, it can be added to an instance of `TBoldExternalVariablelist` and sent to [EvaluateExpression](TBoldElement.md#evaluateexpression). One available implementation of this class is the class used for `TBoldOCLVariables` (`TBoldHandleBasedExternalVariable`)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Name](#name) | The name of the variable | read-only |
| [Value](#value) | Returns the value of the variable. | read-only |
| [ValueType](#valuetype) | Returns the value type of the variable | read-only |

### Name

```delphi
property Name: String;
```

The name of the variable

### Value

```delphi
property Value: TBoldElement;
```

Returns the value of the variable.

### ValueType

```delphi
property ValueType: TBoldElementTypeInfo;
```

Returns the value type of the variable

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) |  |  |
| [GetValue](#getvalue) | Abstract method to return the value of the variable | protected, abstract |
| [GetValueType](#getvaluetype) |  | protected, abstract |

### Create

```delphi
constructor Create(const Name: String);
```

### GetValue

```delphi
function GetValue: TBoldElement; virtual; abstract;
```

Override this method to define how the variable stores the value. It might store the value directly in a private field variable, or it might store for example a handle from which the value is retrieved

### GetValueType

```delphi
function GetValueType: TBoldElementTypeInfo; virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
