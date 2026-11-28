object dmModelEditor: TdmModelEditor
  Height = 304
  Width = 400
  object rhModel: TBoldReferenceHandle
    StaticValueTypeName = 'UMLModel'
    Left = 40
    Top = 24
  end
  object lhClasses: TBoldListHandle
    RootHandle = rhModel
    Expression = 'classes'
    Left = 40
    Top = 88
  end
  object lhAttributes: TBoldListHandle
    RootHandle = lhClasses
    Expression = 'feature->filterOnType(UMLAttribute)'
    Left = 40
    Top = 152
  end
  object bdsClasses: TBoldDataSet
    FieldDescriptions = <
      item
        BoldProperties.Expression = 'name'
        FieldName = 'Name'
      end
      item
        BoldProperties.Expression = 'superclass.name'
        FieldName = 'Superclass'
      end
      item
        BoldProperties.Expression = 'isAbstract'
        FieldName = 'Abstract'
      end
      item
        BoldProperties.Expression = 'persistent'
        FieldName = 'Persistent'
      end>
    AutoOpen = False
    BoldHandle = lhClasses
    Left = 160
    Top = 88
  end
  object bdsAttributes: TBoldDataSet
    FieldDescriptions = <
      item
        BoldProperties.Expression = 'name'
        FieldName = 'Name'
      end
      item
        BoldProperties.Expression = 'typeName'
        FieldName = 'Type'
      end
      item
        BoldProperties.Expression = 'persistent'
        FieldName = 'Persistent'
      end
      item
        BoldProperties.Expression = 'derived'
        FieldName = 'Derived'
      end>
    AutoOpen = False
    BoldHandle = lhAttributes
    Left = 160
    Top = 152
  end
  object lhViolations: TBoldListHandle
    RootHandle = rhModel
    Expression = 'validator.violation->orderdescending(severity)'
    Left = 40
    Top = 216
  end
  object bdsViolations: TBoldDataSet
    FieldDescriptions = <
      item
        BoldProperties.Expression = 'severity.asString'
        FieldName = 'Severity'
      end
      item
        BoldProperties.Expression = 'description'
        FieldName = 'Description'
      end
      item
        BoldProperties.Expression = 'modelElement.name'
        FieldName = 'Element'
      end>
    AutoOpen = False
    BoldHandle = lhViolations
    Left = 160
    Top = 216
  end
end
