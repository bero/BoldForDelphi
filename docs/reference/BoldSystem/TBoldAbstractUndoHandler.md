# TBoldAbstractUndoHandler

Bold-internal

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractUndoHandler = class(TBoldSystemExtension)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. [TBoldSystemExtension](TBoldSystemExtension.md)
6. TBoldAbstractUndoHandler
7. **Direct subclasses**
8. `TBoldUndoHandler`

## Description

Bold-internal

## Methods

| Name | Summary | Notes |
|---|---|---|
| [ApplytranslationList](#applytranslationlist) |  | abstract |
| [DeleteObject](#deleteobject) |  | protected |
| [GetControllerForMember](#getcontrollerformember) |  | protected |
| [HandleMember](#handlemember) |  | abstract |
| [HandleObject](#handleobject) |  | abstract |
| [PrepareUpdate](#prepareupdate) |  | abstract |

### ApplytranslationList

```delphi
procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); virtual; abstract;
```

### DeleteObject

```delphi
procedure DeleteObject(BoldObject: TBoldObject);
```

### GetControllerForMember

```delphi
class function GetControllerForMember(Member: TBoldMember): TBoldAbstractController;
```

### HandleMember

```delphi
procedure HandleMember(ObjectContents: IBoldObjectContents; MemberIndex: integer; MemberValue: IBoldValue); virtual; abstract;
```

### HandleObject

```delphi
procedure HandleObject(Obj: IBoldObjectContents; RegardAsExisting: Boolean); virtual; abstract;
```

### PrepareUpdate

```delphi
procedure PrepareUpdate(const ObjectList: TBoldObjectList); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
