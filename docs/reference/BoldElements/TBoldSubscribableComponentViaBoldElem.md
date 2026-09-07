# TBoldSubscribableComponentViaBoldElem

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldSubscribableComponentViaBoldElem = class(TBoldSubscribableComponent)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. TBoldSubscribableComponentViaBoldElem
4. **Direct subclasses**
5. `TBoldComparer`
6. `TBoldFilter`
7. `TBoldManipulator`
8. `TBoldPlaceableSubscriber`
9. `TBoldRenderer`

## Description

When a component is placed on a form in the Delphi, the IDE will automatically include the units for a component and all its direct ancestors. It doesn't however pull in the units needed for the parameters of the events.

This class is a trick used to pull in the BoldElements unit automatically when needed.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
