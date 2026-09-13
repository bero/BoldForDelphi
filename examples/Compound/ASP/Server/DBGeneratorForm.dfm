object frmDBGen: TfrmDBGen
  Left = 428
  Top = 226
  Caption = ' Database Generator for ASP example'
  ClientHeight = 103
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 24
    Top = 40
    Width = 121
    Height = 25
    Caption = 'Create DB'
    TabOrder = 0
    OnClick = Button1Click
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = dmModel.bmASPDemo
    ClockLogGranularity = '0:0:0.0'
    Left = 208
    Top = 32
  end
end
