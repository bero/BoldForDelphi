# TBoldSystemTypeInfoHandle

This handle holds the runtime information needed by a TBoldSystemHandle.

**Unit**: [BoldHandles](index.md)

## Declaration

```delphi
TBoldSystemTypeInfoHandle = class(TBoldElementHandle)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](TBoldElementHandle.md)
4. TBoldSystemTypeInfoHandle

## Description

The information is a efficient representation of the information in the UML-model, heavily optimized for speed.

The type information is created from the model referenced by [BoldModel](TBoldSystemTypeInfoHandle.md#boldmodel). Several systems can share the same `TBoldSystemTypeInfoHandle`. In this case, they will be type compatible in the Bold sense of the word.

**Bold events**  
`TBoldSystemTypeInfoHandle` is a subclass of TBoldSubscribableComponent, and can therefore by subscribed to using `AddSmallSubscription`. A `TBoldSystemTypeInfoHandle`. can send the following events:

- **beDestroying**: Sent when the handle is about to be destroyed
- **beValueChanged**: Sent if it receives any change from the [BoldModel](TBoldSystemTypeInfoHandle.md#boldmodel) component

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldModel](#boldmodel) | Link to the TBoldModel component |  |
| [CheckCodeCheckSum](#checkcodechecksum) | Decides if the checksum in the generated code is honoured |  |
| [RegionDefinitions](#regiondefinitions) | The internal representation of the regions defined in the model. | read-only |
| [UseGeneratedCode](#usegeneratedcode) | Specifies if generated code is to be used. |  |

### BoldModel

```delphi
property BoldModel: TBoldAbstractModel;
```

This property must point to a `TBoldModel` that holds the UML model for the system to be used.

### CheckCodeCheckSum

```delphi
property CheckCodeCheckSum: Boolean;
```

When code is generated, and [UseGeneratedCode](TBoldSystemTypeInfoHandle.md#usegeneratedcode) is checked, the checksum of the generated code is compared with the checksum of the model at system startup.

In some cases during the debug phase, you may want to do changes that you figure will not require regeneration of code, but nevertheless changes the checksum. Setting this property to `False` will prevent the checksums from being compared.

| **Note** |
|---|

| Changing this property to `False` is intended only as a debugging tool. Our recommendation is to check the checksum always, especially in production code! You may even want to write some code that brings up a warning dialog if the `CheckCodeCheckSum` is false and execute this "sanity checker" at application startup. |
|---|

### RegionDefinitions

```delphi
property RegionDefinitions: TBoldRegionDefinitions;
```

The internal representation of the regions defined in the model. See Related Topics for a description of the region definition language.

### UseGeneratedCode

```delphi
property UseGeneratedCode: Boolean;
```

If this property is true the system will expect the project to include a unit containing generated code for the business-classes, allowing the use of Delphi-types, and user-written code. This is the most common case.

If the property is set to false, all objects will be instantiated as [TBoldObject](../BoldSystem/TBoldObject.md)s, and all lists of objects as [TBoldObjectList](../BoldSystem/TBoldObjectList.md)s. In this case all evaluation must be done using OCL, or via methods and properties on the above classes only, such as TBoldObject.[BoldMemberByExpressionName](../BoldSystem/TBoldObject.md#boldmemberbyexpressionname).

New objects can be created with TBoldSystem.CreateNewObjectByExpressionName

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Creates an instance of TBoldSystemTypeInfoHandle | override |
| [Destroy](#destroy) | Destroys an instance of TBoldSystemTypeInfoHandle | override |
| [GetStaticBoldType](#getstaticboldtype) | Overrides TBoldElementHandle.GetStaticBoldType | protected, override |
| [GetStaticSystemTypeInfo](#getstaticsystemtypeinfo) | Overrides TBoldElementHandle.GetStaticSystemTypeInfo | protected, override |
| [GetValue](#getvalue) | Overrides TBoldElementHandle.GetValue | protected, override |
| [InstallOclDefinitionLookUp](#installocldefinitionlookup) | Bold-internal |  |
| [RefersToComponent](#referstocomponent) | Determines if the TBoldSystemTypeHandle refers to the component in the parameter. | override |

### Create

```delphi
constructor Create(owner: TComponent); override;
```

Use `Create` to create an instance of `TBoldSystemTypeInfoHandle`.

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

`Destroy` destroys an instance of a `TBoldSystemTypeInfoHandle`. Do not call `Destroy` directly, use Free instead.

### GetStaticBoldType

```delphi
function GetStaticBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

Overrides [GetStaticBoldType](TBoldElementHandle.md#getstaticboldtype)

### GetStaticSystemTypeInfo

```delphi
function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override; See also Ancestor Method
```

Overrides [GetStaticSystemTypeInfo](TBoldElementHandle.md#getstaticsystemtypeinfo)

### GetValue

```delphi
function GetValue: TBoldElement; override; See also Ancestor Method
```

Overrides [GetValue](TBoldElementHandle.md#getvalue)

### InstallOclDefinitionLookUp

```delphi
procedure InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
```

Bold-internal

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override; See also Ancestor Method
```

`RefersToComponent` is used internally to avoid circular references.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
