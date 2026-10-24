object dmInvoicing: TdmInvoicing
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 200
  Width = 320
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Left = 40
    Top = 32
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 40
    Top = 104
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    Left = 176
    Top = 32
  end
end
