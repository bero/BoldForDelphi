unit jehoBoldTestDataModule;

{ The data module with the jehoBCBoldTest model (dmjehoBoldTest.dfm), shared by
  the Delphi and the Free Pascal test builds. Both compilers embed the same
  .dfm through a resource directive, and TDataModule.Create loads it; TDataModule is declared in
  Classes on both, so no VCL unit is needed. Using this unit also links the
  generated jehoBCBoldTest classes, without which the system cannot activate. }

interface

uses
  Classes,
  BoldHandles,
  BoldSystemHandle,
  BoldModel,
  BoldTypeNameHandle,
  BoldListHandle;

type
  TjehodmBoldTest = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    BoldListHandle1: TBoldListHandle;
  end;

implementation

uses
  jehoBCBoldTest;

{$R dmjehoBoldTest.dfm}

end.
