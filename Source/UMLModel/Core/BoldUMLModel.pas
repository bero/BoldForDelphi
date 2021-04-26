
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModel;

{$DEFINE BoldUMLModel_unitheader}
{$INCLUDE BoldUMLModel_Interface.inc}

{ Includefile for methodimplementations }

{$INCLUDE BoldUMLModelValidation.inc}
{$INCLUDE BoldUMLModel.inc}

const
  BoldMemberAssertInvalidObjectType: string = 'Object of singlelink (%s.%s) is of wrong type (is %s, should be %s)';

{ TUMLModelRoot }

procedure TUMLModelRootList.Add(NewObject: TUMLModelRoot);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLModelRootList.IndexOf(anObject: TUMLModelRoot): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLModelRootList.Includes(anObject: TUMLModelRoot) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLModelRootList.AddNew: TUMLModelRoot;
begin
  result := TUMLModelRoot(InternalAddNew);
end;

procedure TUMLModelRootList.Insert(index: Integer; NewObject: TUMLModelRoot);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLModelRootList.GetBoldObject(index: Integer): TUMLModelRoot;
begin
  result := TUMLModelRoot(GetElement(index));
end;

procedure TUMLModelRootList.SetBoldObject(index: Integer; NewObject: TUMLModelRoot);
begin;
  SetElement(index, NewObject);
end;

{ Targumentstimulus1 }

function Targumentstimulus1._Get_M_stimulus1: TBoldObjectReference;
begin
  assert(ValidateMember('Targumentstimulus1', 'stimulus1', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Targumentstimulus1._Getstimulus1: TUMLStimulus;
begin
  Result := TUMLStimulus(M_stimulus1.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStimulus), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'stimulus1', Result.ClassName, 'TUMLStimulus']));
end;

function Targumentstimulus1._Get_M_argument: TBoldObjectReference;
begin
  assert(ValidateMember('Targumentstimulus1', 'argument', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Targumentstimulus1._Getargument: TUMLInstance;
begin
  Result := TUMLInstance(M_argument.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'argument', Result.ClassName, 'TUMLInstance']));
end;

procedure Targumentstimulus1List.Add(NewObject: Targumentstimulus1);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function Targumentstimulus1List.IndexOf(anObject: Targumentstimulus1): Integer;
begin
  result := IndexOfElement(anObject);
end;

function Targumentstimulus1List.Includes(anObject: Targumentstimulus1) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function Targumentstimulus1List.AddNew: Targumentstimulus1;
begin
  result := Targumentstimulus1(InternalAddNew);
end;

procedure Targumentstimulus1List.Insert(index: Integer; NewObject: Targumentstimulus1);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function Targumentstimulus1List.GetBoldObject(index: Integer): Targumentstimulus1;
begin
  result := Targumentstimulus1(GetElement(index));
end;

procedure Targumentstimulus1List.SetBoldObject(index: Integer; NewObject: Targumentstimulus1);
begin;
  SetElement(index, NewObject);
end;

{ TassociationEndRoleavailableQualifier }

function TassociationEndRoleavailableQualifier._Get_M_availableQualifier: TBoldObjectReference;
begin
  assert(ValidateMember('TassociationEndRoleavailableQualifier', 'availableQualifier', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TassociationEndRoleavailableQualifier._GetavailableQualifier: TUMLAttribute;
begin
  Result := TUMLAttribute(M_availableQualifier.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAttribute), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'availableQualifier', Result.ClassName, 'TUMLAttribute']));
end;

function TassociationEndRoleavailableQualifier._Get_M_associationEndRole: TBoldObjectReference;
begin
  assert(ValidateMember('TassociationEndRoleavailableQualifier', 'associationEndRole', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TassociationEndRoleavailableQualifier._GetassociationEndRole: TUMLAssociationEndRole;
begin
  Result := TUMLAssociationEndRole(M_associationEndRole.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationEndRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'associationEndRole', Result.ClassName, 'TUMLAssociationEndRole']));
end;

procedure TassociationEndRoleavailableQualifierList.Add(NewObject: TassociationEndRoleavailableQualifier);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TassociationEndRoleavailableQualifierList.IndexOf(anObject: TassociationEndRoleavailableQualifier): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TassociationEndRoleavailableQualifierList.Includes(anObject: TassociationEndRoleavailableQualifier) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TassociationEndRoleavailableQualifierList.AddNew: TassociationEndRoleavailableQualifier;
begin
  result := TassociationEndRoleavailableQualifier(InternalAddNew);
end;

procedure TassociationEndRoleavailableQualifierList.Insert(index: Integer; NewObject: TassociationEndRoleavailableQualifier);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TassociationEndRoleavailableQualifierList.GetBoldObject(index: Integer): TassociationEndRoleavailableQualifier;
begin
  result := TassociationEndRoleavailableQualifier(GetElement(index));
end;

procedure TassociationEndRoleavailableQualifierList.SetBoldObject(index: Integer; NewObject: TassociationEndRoleavailableQualifier);
begin;
  SetElement(index, NewObject);
end;

{ TclassifierclassifierRole_ }

function TclassifierclassifierRole_._Get_M_classifierRole_: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierclassifierRole_', 'classifierRole_', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TclassifierclassifierRole_._GetclassifierRole_: TUMLClassifierRole;
begin
  Result := TUMLClassifierRole(M_classifierRole_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifierRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'classifierRole_', Result.ClassName, 'TUMLClassifierRole']));
end;

function TclassifierclassifierRole_._Get_M_classifier: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierclassifierRole_', 'classifier', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TclassifierclassifierRole_._Getclassifier: TUMLClassifier;
begin
  Result := TUMLClassifier(M_classifier.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'classifier', Result.ClassName, 'TUMLClassifier']));
end;

procedure TclassifierclassifierRole_List.Add(NewObject: TclassifierclassifierRole_);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TclassifierclassifierRole_List.IndexOf(anObject: TclassifierclassifierRole_): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TclassifierclassifierRole_List.Includes(anObject: TclassifierclassifierRole_) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TclassifierclassifierRole_List.AddNew: TclassifierclassifierRole_;
begin
  result := TclassifierclassifierRole_(InternalAddNew);
end;

procedure TclassifierclassifierRole_List.Insert(index: Integer; NewObject: TclassifierclassifierRole_);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TclassifierclassifierRole_List.GetBoldObject(index: Integer): TclassifierclassifierRole_;
begin
  result := TclassifierclassifierRole_(GetElement(index));
end;

procedure TclassifierclassifierRole_List.SetBoldObject(index: Integer; NewObject: TclassifierclassifierRole_);
begin;
  SetElement(index, NewObject);
end;

{ TclassifierInStateinState }

function TclassifierInStateinState._Get_M_inState: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierInStateinState', 'inState', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TclassifierInStateinState._GetinState: TUMLState;
begin
  Result := TUMLState(M_inState.BoldObject);
  assert(not assigned(Result) or (Result is TUMLState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'inState', Result.ClassName, 'TUMLState']));
end;

function TclassifierInStateinState._Get_M_classifierInState: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierInStateinState', 'classifierInState', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TclassifierInStateinState._GetclassifierInState: TUMLClassifierInState;
begin
  Result := TUMLClassifierInState(M_classifierInState.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifierInState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'classifierInState', Result.ClassName, 'TUMLClassifierInState']));
end;

procedure TclassifierInStateinStateList.Add(NewObject: TclassifierInStateinState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TclassifierInStateinStateList.IndexOf(anObject: TclassifierInStateinState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TclassifierInStateinStateList.Includes(anObject: TclassifierInStateinState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TclassifierInStateinStateList.AddNew: TclassifierInStateinState;
begin
  result := TclassifierInStateinState(InternalAddNew);
end;

procedure TclassifierInStateinStateList.Insert(index: Integer; NewObject: TclassifierInStateinState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TclassifierInStateinStateList.GetBoldObject(index: Integer): TclassifierInStateinState;
begin
  result := TclassifierInStateinState(GetElement(index));
end;

procedure TclassifierInStateinStateList.SetBoldObject(index: Integer; NewObject: TclassifierInStateinState);
begin;
  SetElement(index, NewObject);
end;

{ TclassifierRole_availableFeature }

function TclassifierRole_availableFeature._Get_M_availableFeature: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierRole_availableFeature', 'availableFeature', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TclassifierRole_availableFeature._GetavailableFeature: TUMLFeature;
begin
  Result := TUMLFeature(M_availableFeature.BoldObject);
  assert(not assigned(Result) or (Result is TUMLFeature), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'availableFeature', Result.ClassName, 'TUMLFeature']));
end;

function TclassifierRole_availableFeature._Get_M_classifierRole_: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierRole_availableFeature', 'classifierRole_', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TclassifierRole_availableFeature._GetclassifierRole_: TUMLClassifierRole;
begin
  Result := TUMLClassifierRole(M_classifierRole_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifierRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'classifierRole_', Result.ClassName, 'TUMLClassifierRole']));
end;

procedure TclassifierRole_availableFeatureList.Add(NewObject: TclassifierRole_availableFeature);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TclassifierRole_availableFeatureList.IndexOf(anObject: TclassifierRole_availableFeature): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TclassifierRole_availableFeatureList.Includes(anObject: TclassifierRole_availableFeature) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TclassifierRole_availableFeatureList.AddNew: TclassifierRole_availableFeature;
begin
  result := TclassifierRole_availableFeature(InternalAddNew);
end;

procedure TclassifierRole_availableFeatureList.Insert(index: Integer; NewObject: TclassifierRole_availableFeature);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TclassifierRole_availableFeatureList.GetBoldObject(index: Integer): TclassifierRole_availableFeature;
begin
  result := TclassifierRole_availableFeature(GetElement(index));
end;

procedure TclassifierRole_availableFeatureList.SetBoldObject(index: Integer; NewObject: TclassifierRole_availableFeature);
begin;
  SetElement(index, NewObject);
end;

{ TclassifierRoleavailableContents }

function TclassifierRoleavailableContents._Get_M_availableContents: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierRoleavailableContents', 'availableContents', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TclassifierRoleavailableContents._GetavailableContents: TUMLModelElement;
begin
  Result := TUMLModelElement(M_availableContents.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'availableContents', Result.ClassName, 'TUMLModelElement']));
end;

function TclassifierRoleavailableContents._Get_M_classifierRole: TBoldObjectReference;
begin
  assert(ValidateMember('TclassifierRoleavailableContents', 'classifierRole', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TclassifierRoleavailableContents._GetclassifierRole: TUMLClassifierRole;
begin
  Result := TUMLClassifierRole(M_classifierRole.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifierRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'classifierRole', Result.ClassName, 'TUMLClassifierRole']));
end;

procedure TclassifierRoleavailableContentsList.Add(NewObject: TclassifierRoleavailableContents);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TclassifierRoleavailableContentsList.IndexOf(anObject: TclassifierRoleavailableContents): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TclassifierRoleavailableContentsList.Includes(anObject: TclassifierRoleavailableContents) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TclassifierRoleavailableContentsList.AddNew: TclassifierRoleavailableContents;
begin
  result := TclassifierRoleavailableContents(InternalAddNew);
end;

procedure TclassifierRoleavailableContentsList.Insert(index: Integer; NewObject: TclassifierRoleavailableContents);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TclassifierRoleavailableContentsList.GetBoldObject(index: Integer): TclassifierRoleavailableContents;
begin
  result := TclassifierRoleavailableContents(GetElement(index));
end;

procedure TclassifierRoleavailableContentsList.SetBoldObject(index: Integer; NewObject: TclassifierRoleavailableContents);
begin;
  SetElement(index, NewObject);
end;

{ TclientclientDependency }

function TclientclientDependency._Get_M_clientDependency: TBoldObjectReference;
begin
  assert(ValidateMember('TclientclientDependency', 'clientDependency', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TclientclientDependency._GetclientDependency: TUMLDependency;
begin
  Result := TUMLDependency(M_clientDependency.BoldObject);
  assert(not assigned(Result) or (Result is TUMLDependency), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'clientDependency', Result.ClassName, 'TUMLDependency']));
end;

function TclientclientDependency._Get_M_client: TBoldObjectReference;
begin
  assert(ValidateMember('TclientclientDependency', 'client', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TclientclientDependency._Getclient: TUMLModelElement;
begin
  Result := TUMLModelElement(M_client.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'client', Result.ClassName, 'TUMLModelElement']));
end;

procedure TclientclientDependencyList.Add(NewObject: TclientclientDependency);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TclientclientDependencyList.IndexOf(anObject: TclientclientDependency): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TclientclientDependencyList.Includes(anObject: TclientclientDependency) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TclientclientDependencyList.AddNew: TclientclientDependency;
begin
  result := TclientclientDependency(InternalAddNew);
end;

procedure TclientclientDependencyList.Insert(index: Integer; NewObject: TclientclientDependency);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TclientclientDependencyList.GetBoldObject(index: Integer): TclientclientDependency;
begin
  result := TclientclientDependency(GetElement(index));
end;

procedure TclientclientDependencyList.SetBoldObject(index: Integer; NewObject: TclientclientDependency);
begin;
  SetElement(index, NewObject);
end;

{ TcollaborationconstrainingElement }

function TcollaborationconstrainingElement._Get_M_constrainingElement: TBoldObjectReference;
begin
  assert(ValidateMember('TcollaborationconstrainingElement', 'constrainingElement', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TcollaborationconstrainingElement._GetconstrainingElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_constrainingElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'constrainingElement', Result.ClassName, 'TUMLModelElement']));
end;

function TcollaborationconstrainingElement._Get_M_collaboration: TBoldObjectReference;
begin
  assert(ValidateMember('TcollaborationconstrainingElement', 'collaboration', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TcollaborationconstrainingElement._Getcollaboration: TUMLCollaboration;
begin
  Result := TUMLCollaboration(M_collaboration.BoldObject);
  assert(not assigned(Result) or (Result is TUMLCollaboration), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'collaboration', Result.ClassName, 'TUMLCollaboration']));
end;

procedure TcollaborationconstrainingElementList.Add(NewObject: TcollaborationconstrainingElement);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TcollaborationconstrainingElementList.IndexOf(anObject: TcollaborationconstrainingElement): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TcollaborationconstrainingElementList.Includes(anObject: TcollaborationconstrainingElement) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TcollaborationconstrainingElementList.AddNew: TcollaborationconstrainingElement;
begin
  result := TcollaborationconstrainingElement(InternalAddNew);
end;

procedure TcollaborationconstrainingElementList.Insert(index: Integer; NewObject: TcollaborationconstrainingElement);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TcollaborationconstrainingElementList.GetBoldObject(index: Integer): TcollaborationconstrainingElement;
begin
  result := TcollaborationconstrainingElement(GetElement(index));
end;

procedure TcollaborationconstrainingElementList.SetBoldObject(index: Integer; NewObject: TcollaborationconstrainingElement);
begin;
  SetElement(index, NewObject);
end;

{ TcommentannotatedElement }

function TcommentannotatedElement._Get_M_annotatedElement: TBoldObjectReference;
begin
  assert(ValidateMember('TcommentannotatedElement', 'annotatedElement', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TcommentannotatedElement._GetannotatedElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_annotatedElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'annotatedElement', Result.ClassName, 'TUMLModelElement']));
end;

function TcommentannotatedElement._Get_M_comment: TBoldObjectReference;
begin
  assert(ValidateMember('TcommentannotatedElement', 'comment', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TcommentannotatedElement._Getcomment: TUMLComment;
begin
  Result := TUMLComment(M_comment.BoldObject);
  assert(not assigned(Result) or (Result is TUMLComment), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'comment', Result.ClassName, 'TUMLComment']));
end;

procedure TcommentannotatedElementList.Add(NewObject: TcommentannotatedElement);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TcommentannotatedElementList.IndexOf(anObject: TcommentannotatedElement): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TcommentannotatedElementList.Includes(anObject: TcommentannotatedElement) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TcommentannotatedElementList.AddNew: TcommentannotatedElement;
begin
  result := TcommentannotatedElement(InternalAddNew);
end;

procedure TcommentannotatedElementList.Insert(index: Integer; NewObject: TcommentannotatedElement);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TcommentannotatedElementList.GetBoldObject(index: Integer): TcommentannotatedElement;
begin
  result := TcommentannotatedElement(GetElement(index));
end;

procedure TcommentannotatedElementList.SetBoldObject(index: Integer; NewObject: TcommentannotatedElement);
begin;
  SetElement(index, NewObject);
end;

{ TconstrainedElementconstraint }

function TconstrainedElementconstraint._Get_M_constraint: TBoldObjectReference;
begin
  assert(ValidateMember('TconstrainedElementconstraint', 'constraint', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TconstrainedElementconstraint._Getconstraint: TUMLConstraint;
begin
  Result := TUMLConstraint(M_constraint.BoldObject);
  assert(not assigned(Result) or (Result is TUMLConstraint), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'constraint', Result.ClassName, 'TUMLConstraint']));
end;

function TconstrainedElementconstraint._Get_M_constrainedElement: TBoldObjectReference;
begin
  assert(ValidateMember('TconstrainedElementconstraint', 'constrainedElement', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TconstrainedElementconstraint._GetconstrainedElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_constrainedElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'constrainedElement', Result.ClassName, 'TUMLModelElement']));
end;

procedure TconstrainedElementconstraintList.Add(NewObject: TconstrainedElementconstraint);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TconstrainedElementconstraintList.IndexOf(anObject: TconstrainedElementconstraint): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TconstrainedElementconstraintList.Includes(anObject: TconstrainedElementconstraint) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TconstrainedElementconstraintList.AddNew: TconstrainedElementconstraint;
begin
  result := TconstrainedElementconstraint(InternalAddNew);
end;

procedure TconstrainedElementconstraintList.Insert(index: Integer; NewObject: TconstrainedElementconstraint);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TconstrainedElementconstraintList.GetBoldObject(index: Integer): TconstrainedElementconstraint;
begin
  result := TconstrainedElementconstraint(GetElement(index));
end;

procedure TconstrainedElementconstraintList.SetBoldObject(index: Integer; NewObject: TconstrainedElementconstraint);
begin;
  SetElement(index, NewObject);
end;

{ Tcontentspartition }

function Tcontentspartition._Get_M_partition: TBoldObjectReference;
begin
  assert(ValidateMember('Tcontentspartition', 'partition', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tcontentspartition._Getpartition: TUMLPartition;
begin
  Result := TUMLPartition(M_partition.BoldObject);
  assert(not assigned(Result) or (Result is TUMLPartition), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'partition', Result.ClassName, 'TUMLPartition']));
end;

function Tcontentspartition._Get_M_contents: TBoldObjectReference;
begin
  assert(ValidateMember('Tcontentspartition', 'contents', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tcontentspartition._Getcontents: TUMLModelElement;
begin
  Result := TUMLModelElement(M_contents.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'contents', Result.ClassName, 'TUMLModelElement']));
end;

procedure TcontentspartitionList.Add(NewObject: Tcontentspartition);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TcontentspartitionList.IndexOf(anObject: Tcontentspartition): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TcontentspartitionList.Includes(anObject: Tcontentspartition) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TcontentspartitionList.AddNew: Tcontentspartition;
begin
  result := Tcontentspartition(InternalAddNew);
end;

procedure TcontentspartitionList.Insert(index: Integer; NewObject: Tcontentspartition);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TcontentspartitionList.GetBoldObject(index: Integer): Tcontentspartition;
begin
  result := Tcontentspartition(GetElement(index));
end;

procedure TcontentspartitionList.SetBoldObject(index: Integer; NewObject: Tcontentspartition);
begin;
  SetElement(index, NewObject);
end;

{ TcontextraisedSignal }

function TcontextraisedSignal._Get_M_raisedSignal: TBoldObjectReference;
begin
  assert(ValidateMember('TcontextraisedSignal', 'raisedSignal', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TcontextraisedSignal._GetraisedSignal: TUMLSignal;
begin
  Result := TUMLSignal(M_raisedSignal.BoldObject);
  assert(not assigned(Result) or (Result is TUMLSignal), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'raisedSignal', Result.ClassName, 'TUMLSignal']));
end;

function TcontextraisedSignal._Get_M_context: TBoldObjectReference;
begin
  assert(ValidateMember('TcontextraisedSignal', 'context', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TcontextraisedSignal._Getcontext: TUMLBehavioralFeature;
begin
  Result := TUMLBehavioralFeature(M_context.BoldObject);
  assert(not assigned(Result) or (Result is TUMLBehavioralFeature), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'context', Result.ClassName, 'TUMLBehavioralFeature']));
end;

procedure TcontextraisedSignalList.Add(NewObject: TcontextraisedSignal);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TcontextraisedSignalList.IndexOf(anObject: TcontextraisedSignal): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TcontextraisedSignalList.Includes(anObject: TcontextraisedSignal) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TcontextraisedSignalList.AddNew: TcontextraisedSignal;
begin
  result := TcontextraisedSignal(InternalAddNew);
end;

procedure TcontextraisedSignalList.Insert(index: Integer; NewObject: TcontextraisedSignal);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TcontextraisedSignalList.GetBoldObject(index: Integer): TcontextraisedSignal;
begin
  result := TcontextraisedSignal(GetElement(index));
end;

procedure TcontextraisedSignalList.SetBoldObject(index: Integer; NewObject: TcontextraisedSignal);
begin;
  SetElement(index, NewObject);
end;

{ TdeploymentLocationresident }

function TdeploymentLocationresident._Get_M_resident: TBoldObjectReference;
begin
  assert(ValidateMember('TdeploymentLocationresident', 'resident', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TdeploymentLocationresident._Getresident: TUMLComponent;
begin
  Result := TUMLComponent(M_resident.BoldObject);
  assert(not assigned(Result) or (Result is TUMLComponent), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'resident', Result.ClassName, 'TUMLComponent']));
end;

function TdeploymentLocationresident._Get_M_deploymentLocation: TBoldObjectReference;
begin
  assert(ValidateMember('TdeploymentLocationresident', 'deploymentLocation', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TdeploymentLocationresident._GetdeploymentLocation: TUMLNode;
begin
  Result := TUMLNode(M_deploymentLocation.BoldObject);
  assert(not assigned(Result) or (Result is TUMLNode), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'deploymentLocation', Result.ClassName, 'TUMLNode']));
end;

procedure TdeploymentLocationresidentList.Add(NewObject: TdeploymentLocationresident);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TdeploymentLocationresidentList.IndexOf(anObject: TdeploymentLocationresident): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TdeploymentLocationresidentList.Includes(anObject: TdeploymentLocationresident) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TdeploymentLocationresidentList.AddNew: TdeploymentLocationresident;
begin
  result := TdeploymentLocationresident(InternalAddNew);
end;

procedure TdeploymentLocationresidentList.Insert(index: Integer; NewObject: TdeploymentLocationresident);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TdeploymentLocationresidentList.GetBoldObject(index: Integer): TdeploymentLocationresident;
begin
  result := TdeploymentLocationresident(GetElement(index));
end;

procedure TdeploymentLocationresidentList.SetBoldObject(index: Integer; NewObject: TdeploymentLocationresident);
begin;
  SetElement(index, NewObject);
end;

{ TUMLElement }

procedure TUMLElementList.Add(NewObject: TUMLElement);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLElementList.IndexOf(anObject: TUMLElement): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLElementList.Includes(anObject: TUMLElement) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLElementList.AddNew: TUMLElement;
begin
  result := TUMLElement(InternalAddNew);
end;

procedure TUMLElementList.Insert(index: Integer; NewObject: TUMLElement);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLElementList.GetBoldObject(index: Integer): TUMLElement;
begin
  result := TUMLElement(GetElement(index));
end;

procedure TUMLElementList.SetBoldObject(index: Integer; NewObject: TUMLElement);
begin;
  SetElement(index, NewObject);
end;

{ TUMLElementImport }

function TUMLElementImport._Get_M_visibility: TBAVisibilityKind;
begin
  assert(ValidateMember('TUMLElementImport', 'visibility', 0, TBAVisibilityKind));
  Result := TBAVisibilityKind(BoldMembers[0]);
end;

function TUMLElementImport._Getvisibility: TVisibilityKind;
begin
  Result := M_visibility.AsVisibilityKind;
end;

procedure TUMLElementImport._Setvisibility(const NewValue: TVisibilityKind);
begin
  M_visibility.AsVisibilityKind := NewValue;
end;

function TUMLElementImport._Get_M_alias: TBAString;
begin
  assert(ValidateMember('TUMLElementImport', 'alias', 1, TBAString));
  Result := TBAString(BoldMembers[1]);
end;

function TUMLElementImport._Getalias: String;
begin
  Result := M_alias.AsString;
end;

procedure TUMLElementImport._Setalias(const NewValue: String);
begin
  M_alias.AsString := NewValue;
end;

function TUMLElementImport._Get_M_package: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLElementImport', 'package', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TUMLElementImport._Getpackage: TUMLPackage;
begin
  Result := TUMLPackage(M_package.BoldObject);
  assert(not assigned(Result) or (Result is TUMLPackage), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'package', Result.ClassName, 'TUMLPackage']));
end;

procedure TUMLElementImport._Setpackage(const value: TUMLPackage);
begin
  M_package.BoldObject := value;
end;

function TUMLElementImport._Get_M_modelElement: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLElementImport', 'modelElement', 3, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[3]);
end;

function TUMLElementImport._GetmodelElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_modelElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'modelElement', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLElementImport._SetmodelElement(const value: TUMLModelElement);
begin
  M_modelElement.BoldObject := value;
end;

procedure TUMLElementImportList.Add(NewObject: TUMLElementImport);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLElementImportList.IndexOf(anObject: TUMLElementImport): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLElementImportList.Includes(anObject: TUMLElementImport) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLElementImportList.AddNew: TUMLElementImport;
begin
  result := TUMLElementImport(InternalAddNew);
end;

procedure TUMLElementImportList.Insert(index: Integer; NewObject: TUMLElementImport);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLElementImportList.GetBoldObject(index: Integer): TUMLElementImport;
begin
  result := TUMLElementImport(GetElement(index));
end;

procedure TUMLElementImportList.SetBoldObject(index: Integer; NewObject: TUMLElementImport);
begin;
  SetElement(index, NewObject);
end;

{ TUMLElementResidence }

function TUMLElementResidence._Get_M_visibility: TBAVisibilityKind;
begin
  assert(ValidateMember('TUMLElementResidence', 'visibility', 0, TBAVisibilityKind));
  Result := TBAVisibilityKind(BoldMembers[0]);
end;

function TUMLElementResidence._Getvisibility: TVisibilityKind;
begin
  Result := M_visibility.AsVisibilityKind;
end;

procedure TUMLElementResidence._Setvisibility(const NewValue: TVisibilityKind);
begin
  M_visibility.AsVisibilityKind := NewValue;
end;

function TUMLElementResidence._Get_M_residentElement: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLElementResidence', 'residentElement', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TUMLElementResidence._GetresidentElement: TUMLComponent;
begin
  Result := TUMLComponent(M_residentElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLComponent), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'residentElement', Result.ClassName, 'TUMLComponent']));
end;

procedure TUMLElementResidence._SetresidentElement(const value: TUMLComponent);
begin
  M_residentElement.BoldObject := value;
end;

function TUMLElementResidence._Get_M_residence: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLElementResidence', 'residence', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TUMLElementResidence._Getresidence: TUMLModelElement;
begin
  Result := TUMLModelElement(M_residence.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'residence', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLElementResidence._Setresidence(const value: TUMLModelElement);
begin
  M_residence.BoldObject := value;
end;

procedure TUMLElementResidenceList.Add(NewObject: TUMLElementResidence);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLElementResidenceList.IndexOf(anObject: TUMLElementResidence): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLElementResidenceList.Includes(anObject: TUMLElementResidence) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLElementResidenceList.AddNew: TUMLElementResidence;
begin
  result := TUMLElementResidence(InternalAddNew);
end;

procedure TUMLElementResidenceList.Insert(index: Integer; NewObject: TUMLElementResidence);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLElementResidenceList.GetBoldObject(index: Integer): TUMLElementResidence;
begin
  result := TUMLElementResidence(GetElement(index));
end;

procedure TUMLElementResidenceList.SetBoldObject(index: Integer; NewObject: TUMLElementResidence);
begin;
  SetElement(index, NewObject);
end;

{ TextensionPointextend }

function TextensionPointextend._Get_M_extend: TBoldObjectReference;
begin
  assert(ValidateMember('TextensionPointextend', 'extend', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TextensionPointextend._Getextend: TUMLExtend;
begin
  Result := TUMLExtend(M_extend.BoldObject);
  assert(not assigned(Result) or (Result is TUMLExtend), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'extend', Result.ClassName, 'TUMLExtend']));
end;

function TextensionPointextend._Get_M_extensionPoint: TBoldObjectReference;
begin
  assert(ValidateMember('TextensionPointextend', 'extensionPoint', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TextensionPointextend._GetextensionPoint: TUMLExtensionPoint;
begin
  Result := TUMLExtensionPoint(M_extensionPoint.BoldObject);
  assert(not assigned(Result) or (Result is TUMLExtensionPoint), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'extensionPoint', Result.ClassName, 'TUMLExtensionPoint']));
end;

procedure TextensionPointextendList.Add(NewObject: TextensionPointextend);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TextensionPointextendList.IndexOf(anObject: TextensionPointextend): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TextensionPointextendList.Includes(anObject: TextensionPointextend) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TextensionPointextendList.AddNew: TextensionPointextend;
begin
  result := TextensionPointextend(InternalAddNew);
end;

procedure TextensionPointextendList.Insert(index: Integer; NewObject: TextensionPointextend);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TextensionPointextendList.GetBoldObject(index: Integer): TextensionPointextend;
begin
  result := TextensionPointextend(GetElement(index));
end;

procedure TextensionPointextendList.SetBoldObject(index: Integer; NewObject: TextensionPointextend);
begin;
  SetElement(index, NewObject);
end;

{ Tinstanceclassifier }

function Tinstanceclassifier._Get_M_classifier: TBoldObjectReference;
begin
  assert(ValidateMember('Tinstanceclassifier', 'classifier', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tinstanceclassifier._Getclassifier: TUMLClassifier;
begin
  Result := TUMLClassifier(M_classifier.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'classifier', Result.ClassName, 'TUMLClassifier']));
end;

function Tinstanceclassifier._Get_M_instance: TBoldObjectReference;
begin
  assert(ValidateMember('Tinstanceclassifier', 'instance', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tinstanceclassifier._Getinstance: TUMLInstance;
begin
  Result := TUMLInstance(M_instance.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'instance', Result.ClassName, 'TUMLInstance']));
end;

procedure TinstanceclassifierList.Add(NewObject: Tinstanceclassifier);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TinstanceclassifierList.IndexOf(anObject: Tinstanceclassifier): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TinstanceclassifierList.Includes(anObject: Tinstanceclassifier) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TinstanceclassifierList.AddNew: Tinstanceclassifier;
begin
  result := Tinstanceclassifier(InternalAddNew);
end;

procedure TinstanceclassifierList.Insert(index: Integer; NewObject: Tinstanceclassifier);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TinstanceclassifierList.GetBoldObject(index: Integer): Tinstanceclassifier;
begin
  result := Tinstanceclassifier(GetElement(index));
end;

procedure TinstanceclassifierList.SetBoldObject(index: Integer; NewObject: Tinstanceclassifier);
begin;
  SetElement(index, NewObject);
end;

{ Tparameterstate }

function Tparameterstate._Get_M_state: TBoldObjectReference;
begin
  assert(ValidateMember('Tparameterstate', 'state', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tparameterstate._Getstate: TUMLObjectFlowState;
begin
  Result := TUMLObjectFlowState(M_state.BoldObject);
  assert(not assigned(Result) or (Result is TUMLObjectFlowState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'state', Result.ClassName, 'TUMLObjectFlowState']));
end;

function Tparameterstate._Get_M_parameter: TBoldObjectReference;
begin
  assert(ValidateMember('Tparameterstate', 'parameter', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tparameterstate._Getparameter: TUMLParameter;
begin
  Result := TUMLParameter(M_parameter.BoldObject);
  assert(not assigned(Result) or (Result is TUMLParameter), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'parameter', Result.ClassName, 'TUMLParameter']));
end;

procedure TparameterstateList.Add(NewObject: Tparameterstate);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TparameterstateList.IndexOf(anObject: Tparameterstate): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TparameterstateList.Includes(anObject: Tparameterstate) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TparameterstateList.AddNew: Tparameterstate;
begin
  result := Tparameterstate(InternalAddNew);
end;

procedure TparameterstateList.Insert(index: Integer; NewObject: Tparameterstate);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TparameterstateList.GetBoldObject(index: Integer): Tparameterstate;
begin
  result := Tparameterstate(GetElement(index));
end;

procedure TparameterstateList.SetBoldObject(index: Integer; NewObject: Tparameterstate);
begin;
  SetElement(index, NewObject);
end;

{ Tparticipantspecification }

function Tparticipantspecification._Get_M_specification: TBoldObjectReference;
begin
  assert(ValidateMember('Tparticipantspecification', 'specification', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tparticipantspecification._Getspecification: TUMLClassifier;
begin
  Result := TUMLClassifier(M_specification.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'specification', Result.ClassName, 'TUMLClassifier']));
end;

function Tparticipantspecification._Get_M_participant: TBoldObjectReference;
begin
  assert(ValidateMember('Tparticipantspecification', 'participant', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tparticipantspecification._Getparticipant: TUMLAssociationEnd;
begin
  Result := TUMLAssociationEnd(M_participant.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationEnd), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'participant', Result.ClassName, 'TUMLAssociationEnd']));
end;

procedure TparticipantspecificationList.Add(NewObject: Tparticipantspecification);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TparticipantspecificationList.IndexOf(anObject: Tparticipantspecification): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TparticipantspecificationList.Includes(anObject: Tparticipantspecification) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TparticipantspecificationList.AddNew: Tparticipantspecification;
begin
  result := Tparticipantspecification(InternalAddNew);
end;

procedure TparticipantspecificationList.Insert(index: Integer; NewObject: Tparticipantspecification);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TparticipantspecificationList.GetBoldObject(index: Integer): Tparticipantspecification;
begin
  result := Tparticipantspecification(GetElement(index));
end;

procedure TparticipantspecificationList.SetBoldObject(index: Integer; NewObject: Tparticipantspecification);
begin;
  SetElement(index, NewObject);
end;

{ Tpredecessormessage3 }

function Tpredecessormessage3._Get_M_message3: TBoldObjectReference;
begin
  assert(ValidateMember('Tpredecessormessage3', 'message3', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tpredecessormessage3._Getmessage3: TUMLMessage;
begin
  Result := TUMLMessage(M_message3.BoldObject);
  assert(not assigned(Result) or (Result is TUMLMessage), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'message3', Result.ClassName, 'TUMLMessage']));
end;

function Tpredecessormessage3._Get_M_predecessor: TBoldObjectReference;
begin
  assert(ValidateMember('Tpredecessormessage3', 'predecessor', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tpredecessormessage3._Getpredecessor: TUMLMessage;
begin
  Result := TUMLMessage(M_predecessor.BoldObject);
  assert(not assigned(Result) or (Result is TUMLMessage), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'predecessor', Result.ClassName, 'TUMLMessage']));
end;

procedure Tpredecessormessage3List.Add(NewObject: Tpredecessormessage3);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function Tpredecessormessage3List.IndexOf(anObject: Tpredecessormessage3): Integer;
begin
  result := IndexOfElement(anObject);
end;

function Tpredecessormessage3List.Includes(anObject: Tpredecessormessage3) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function Tpredecessormessage3List.AddNew: Tpredecessormessage3;
begin
  result := Tpredecessormessage3(InternalAddNew);
end;

procedure Tpredecessormessage3List.Insert(index: Integer; NewObject: Tpredecessormessage3);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function Tpredecessormessage3List.GetBoldObject(index: Integer): Tpredecessormessage3;
begin
  result := Tpredecessormessage3(GetElement(index));
end;

procedure Tpredecessormessage3List.SetBoldObject(index: Integer; NewObject: Tpredecessormessage3);
begin;
  SetElement(index, NewObject);
end;

{ TUMLPresentationElement }

function TUMLPresentationElement._Getsubject: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLPresentationElement', 'subject', 0, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[0]);
end;

function TUMLPresentationElement._Getpresentationsubject: TpresentationsubjectList;
begin
  assert(ValidateMember('TUMLPresentationElement', 'presentationsubject', 1, TpresentationsubjectList));
  Result := TpresentationsubjectList(BoldMembers[1]);
end;

procedure TUMLPresentationElementList.Add(NewObject: TUMLPresentationElement);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLPresentationElementList.IndexOf(anObject: TUMLPresentationElement): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLPresentationElementList.Includes(anObject: TUMLPresentationElement) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLPresentationElementList.AddNew: TUMLPresentationElement;
begin
  result := TUMLPresentationElement(InternalAddNew);
end;

procedure TUMLPresentationElementList.Insert(index: Integer; NewObject: TUMLPresentationElement);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLPresentationElementList.GetBoldObject(index: Integer): TUMLPresentationElement;
begin
  result := TUMLPresentationElement(GetElement(index));
end;

procedure TUMLPresentationElementList.SetBoldObject(index: Integer; NewObject: TUMLPresentationElement);
begin;
  SetElement(index, NewObject);
end;

{ Tpresentationsubject }

function Tpresentationsubject._Get_M_subject: TBoldObjectReference;
begin
  assert(ValidateMember('Tpresentationsubject', 'subject', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tpresentationsubject._Getsubject: TUMLModelElement;
begin
  Result := TUMLModelElement(M_subject.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'subject', Result.ClassName, 'TUMLModelElement']));
end;

function Tpresentationsubject._Get_M_presentation: TBoldObjectReference;
begin
  assert(ValidateMember('Tpresentationsubject', 'presentation', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tpresentationsubject._Getpresentation: TUMLPresentationElement;
begin
  Result := TUMLPresentationElement(M_presentation.BoldObject);
  assert(not assigned(Result) or (Result is TUMLPresentationElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'presentation', Result.ClassName, 'TUMLPresentationElement']));
end;

procedure TpresentationsubjectList.Add(NewObject: Tpresentationsubject);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TpresentationsubjectList.IndexOf(anObject: Tpresentationsubject): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TpresentationsubjectList.Includes(anObject: Tpresentationsubject) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TpresentationsubjectList.AddNew: Tpresentationsubject;
begin
  result := Tpresentationsubject(InternalAddNew);
end;

procedure TpresentationsubjectList.Insert(index: Integer; NewObject: Tpresentationsubject);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TpresentationsubjectList.GetBoldObject(index: Integer): Tpresentationsubject;
begin
  result := Tpresentationsubject(GetElement(index));
end;

procedure TpresentationsubjectList.SetBoldObject(index: Integer; NewObject: Tpresentationsubject);
begin;
  SetElement(index, NewObject);
end;

{ TsourceFlowsource }

function TsourceFlowsource._Get_M_source: TBoldObjectReference;
begin
  assert(ValidateMember('TsourceFlowsource', 'source', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TsourceFlowsource._Getsource: TUMLModelElement;
begin
  Result := TUMLModelElement(M_source.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'source', Result.ClassName, 'TUMLModelElement']));
end;

function TsourceFlowsource._Get_M_sourceFlow: TBoldObjectReference;
begin
  assert(ValidateMember('TsourceFlowsource', 'sourceFlow', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TsourceFlowsource._GetsourceFlow: TUMLFlow;
begin
  Result := TUMLFlow(M_sourceFlow.BoldObject);
  assert(not assigned(Result) or (Result is TUMLFlow), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'sourceFlow', Result.ClassName, 'TUMLFlow']));
end;

procedure TsourceFlowsourceList.Add(NewObject: TsourceFlowsource);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TsourceFlowsourceList.IndexOf(anObject: TsourceFlowsource): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TsourceFlowsourceList.Includes(anObject: TsourceFlowsource) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TsourceFlowsourceList.AddNew: TsourceFlowsource;
begin
  result := TsourceFlowsource(InternalAddNew);
end;

procedure TsourceFlowsourceList.Insert(index: Integer; NewObject: TsourceFlowsource);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TsourceFlowsourceList.GetBoldObject(index: Integer): TsourceFlowsource;
begin
  result := TsourceFlowsource(GetElement(index));
end;

procedure TsourceFlowsourceList.SetBoldObject(index: Integer; NewObject: TsourceFlowsource);
begin;
  SetElement(index, NewObject);
end;

{ TstatedeferrableEvent }

function TstatedeferrableEvent._Get_M_deferrableEvent: TBoldObjectReference;
begin
  assert(ValidateMember('TstatedeferrableEvent', 'deferrableEvent', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TstatedeferrableEvent._GetdeferrableEvent: TUMLEvent;
begin
  Result := TUMLEvent(M_deferrableEvent.BoldObject);
  assert(not assigned(Result) or (Result is TUMLEvent), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'deferrableEvent', Result.ClassName, 'TUMLEvent']));
end;

function TstatedeferrableEvent._Get_M_state: TBoldObjectReference;
begin
  assert(ValidateMember('TstatedeferrableEvent', 'state', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TstatedeferrableEvent._Getstate: TUMLState;
begin
  Result := TUMLState(M_state.BoldObject);
  assert(not assigned(Result) or (Result is TUMLState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'state', Result.ClassName, 'TUMLState']));
end;

procedure TstatedeferrableEventList.Add(NewObject: TstatedeferrableEvent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TstatedeferrableEventList.IndexOf(anObject: TstatedeferrableEvent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TstatedeferrableEventList.Includes(anObject: TstatedeferrableEvent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TstatedeferrableEventList.AddNew: TstatedeferrableEvent;
begin
  result := TstatedeferrableEvent(InternalAddNew);
end;

procedure TstatedeferrableEventList.Insert(index: Integer; NewObject: TstatedeferrableEvent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TstatedeferrableEventList.GetBoldObject(index: Integer): TstatedeferrableEvent;
begin
  result := TstatedeferrableEvent(GetElement(index));
end;

procedure TstatedeferrableEventList.SetBoldObject(index: Integer; NewObject: TstatedeferrableEvent);
begin;
  SetElement(index, NewObject);
end;

{ TsuppliersupplierDependency }

function TsuppliersupplierDependency._Get_M_supplierDependency: TBoldObjectReference;
begin
  assert(ValidateMember('TsuppliersupplierDependency', 'supplierDependency', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TsuppliersupplierDependency._GetsupplierDependency: TUMLDependency;
begin
  Result := TUMLDependency(M_supplierDependency.BoldObject);
  assert(not assigned(Result) or (Result is TUMLDependency), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'supplierDependency', Result.ClassName, 'TUMLDependency']));
end;

function TsuppliersupplierDependency._Get_M_supplier: TBoldObjectReference;
begin
  assert(ValidateMember('TsuppliersupplierDependency', 'supplier', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TsuppliersupplierDependency._Getsupplier: TUMLModelElement;
begin
  Result := TUMLModelElement(M_supplier.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'supplier', Result.ClassName, 'TUMLModelElement']));
end;

procedure TsuppliersupplierDependencyList.Add(NewObject: TsuppliersupplierDependency);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TsuppliersupplierDependencyList.IndexOf(anObject: TsuppliersupplierDependency): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TsuppliersupplierDependencyList.Includes(anObject: TsuppliersupplierDependency) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TsuppliersupplierDependencyList.AddNew: TsuppliersupplierDependency;
begin
  result := TsuppliersupplierDependency(InternalAddNew);
end;

procedure TsuppliersupplierDependencyList.Insert(index: Integer; NewObject: TsuppliersupplierDependency);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TsuppliersupplierDependencyList.GetBoldObject(index: Integer): TsuppliersupplierDependency;
begin
  result := TsuppliersupplierDependency(GetElement(index));
end;

procedure TsuppliersupplierDependencyList.SetBoldObject(index: Integer; NewObject: TsuppliersupplierDependency);
begin;
  SetElement(index, NewObject);
end;

{ TtargetFlowtarget }

function TtargetFlowtarget._Get_M_target: TBoldObjectReference;
begin
  assert(ValidateMember('TtargetFlowtarget', 'target', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TtargetFlowtarget._Gettarget: TUMLModelElement;
begin
  Result := TUMLModelElement(M_target.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'target', Result.ClassName, 'TUMLModelElement']));
end;

function TtargetFlowtarget._Get_M_targetFlow: TBoldObjectReference;
begin
  assert(ValidateMember('TtargetFlowtarget', 'targetFlow', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TtargetFlowtarget._GettargetFlow: TUMLFlow;
begin
  Result := TUMLFlow(M_targetFlow.BoldObject);
  assert(not assigned(Result) or (Result is TUMLFlow), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'targetFlow', Result.ClassName, 'TUMLFlow']));
end;

procedure TtargetFlowtargetList.Add(NewObject: TtargetFlowtarget);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TtargetFlowtargetList.IndexOf(anObject: TtargetFlowtarget): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TtargetFlowtargetList.Includes(anObject: TtargetFlowtarget) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TtargetFlowtargetList.AddNew: TtargetFlowtarget;
begin
  result := TtargetFlowtarget(InternalAddNew);
end;

procedure TtargetFlowtargetList.Insert(index: Integer; NewObject: TtargetFlowtarget);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TtargetFlowtargetList.GetBoldObject(index: Integer): TtargetFlowtarget;
begin
  result := TtargetFlowtarget(GetElement(index));
end;

procedure TtargetFlowtargetList.SetBoldObject(index: Integer; NewObject: TtargetFlowtarget);
begin;
  SetElement(index, NewObject);
end;

{ TUMLTemplateParameter }

function TUMLTemplateParameter._Get_M_modelElement2: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTemplateParameter', 'modelElement2', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TUMLTemplateParameter._GetmodelElement2: TUMLModelElement;
begin
  Result := TUMLModelElement(M_modelElement2.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'modelElement2', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLTemplateParameter._SetmodelElement2(const value: TUMLModelElement);
begin
  M_modelElement2.BoldObject := value;
end;

function TUMLTemplateParameter._Get_M_modelElement: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTemplateParameter', 'modelElement', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TUMLTemplateParameter._GetmodelElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_modelElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'modelElement', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLTemplateParameter._SetmodelElement(const value: TUMLModelElement);
begin
  M_modelElement.BoldObject := value;
end;

function TUMLTemplateParameter._Get_M_defaultElement: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTemplateParameter', 'defaultElement', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TUMLTemplateParameter._GetdefaultElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_defaultElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'defaultElement', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLTemplateParameter._SetdefaultElement(const value: TUMLModelElement);
begin
  M_defaultElement.BoldObject := value;
end;

procedure TUMLTemplateParameterList.Add(NewObject: TUMLTemplateParameter);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLTemplateParameterList.IndexOf(anObject: TUMLTemplateParameter): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLTemplateParameterList.Includes(anObject: TUMLTemplateParameter) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLTemplateParameterList.AddNew: TUMLTemplateParameter;
begin
  result := TUMLTemplateParameter(InternalAddNew);
end;

procedure TUMLTemplateParameterList.Insert(index: Integer; NewObject: TUMLTemplateParameter);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLTemplateParameterList.GetBoldObject(index: Integer): TUMLTemplateParameter;
begin
  result := TUMLTemplateParameter(GetElement(index));
end;

procedure TUMLTemplateParameterList.SetBoldObject(index: Integer; NewObject: TUMLTemplateParameter);
begin;
  SetElement(index, NewObject);
end;

{ TValidator }

function TValidator._Get_M_HighestSeverity: TBASeverity;
begin
  assert(ValidateMember('TValidator', 'HighestSeverity', 0, TBASeverity));
  Result := TBASeverity(BoldMembers[0]);
end;

function TValidator._GetHighestSeverity: TSeverity;
begin
  Result := M_HighestSeverity.AsSeverity;
end;

function TValidator._GetViolation: TViolationList;
begin
  assert(ValidateMember('TValidator', 'Violation', 1, TViolationList));
  Result := TViolationList(BoldMembers[1]);
end;

function TValidator._Get_M_UMLModel: TBoldObjectReference;
begin
  assert(ValidateMember('TValidator', 'UMLModel', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TValidator._GetUMLModel: TUMLModel;
begin
  Result := TUMLModel(M_UMLModel.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModel), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'UMLModel', Result.ClassName, 'TUMLModel']));
end;

procedure TValidator._SetUMLModel(const value: TUMLModel);
begin
  M_UMLModel.BoldObject := value;
end;

procedure TValidatorList.Add(NewObject: TValidator);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TValidatorList.IndexOf(anObject: TValidator): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TValidatorList.Includes(anObject: TValidator) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TValidatorList.AddNew: TValidator;
begin
  result := TValidator(InternalAddNew);
end;

procedure TValidatorList.Insert(index: Integer; NewObject: TValidator);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TValidatorList.GetBoldObject(index: Integer): TValidator;
begin
  result := TValidator(GetElement(index));
end;

procedure TValidatorList.SetBoldObject(index: Integer; NewObject: TValidator);
begin;
  SetElement(index, NewObject);
end;

function TValidator.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    0: result := _HighestSeverity_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

{ TViolation }

function TViolation._Get_M_Description: TBAString;
begin
  assert(ValidateMember('TViolation', 'Description', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TViolation._GetDescription: String;
begin
  Result := M_Description.AsString;
end;

procedure TViolation._SetDescription(const NewValue: String);
begin
  M_Description.AsString := NewValue;
end;

function TViolation._Get_M_Severity: TBASeverity;
begin
  assert(ValidateMember('TViolation', 'Severity', 1, TBASeverity));
  Result := TBASeverity(BoldMembers[1]);
end;

function TViolation._GetSeverity: TSeverity;
begin
  Result := M_Severity.AsSeverity;
end;

procedure TViolation._SetSeverity(const NewValue: TSeverity);
begin
  M_Severity.AsSeverity := NewValue;
end;

function TViolation._Get_M_Validator: TBoldObjectReference;
begin
  assert(ValidateMember('TViolation', 'Validator', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TViolation._GetValidator: TValidator;
begin
  Result := TValidator(M_Validator.BoldObject);
  assert(not assigned(Result) or (Result is TValidator), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Validator', Result.ClassName, 'TValidator']));
end;

procedure TViolation._SetValidator(const value: TValidator);
begin
  M_Validator.BoldObject := value;
end;

function TViolation._Get_M_ModelElement: TBoldObjectReference;
begin
  assert(ValidateMember('TViolation', 'ModelElement', 3, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[3]);
end;

function TViolation._GetModelElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_ModelElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'ModelElement', Result.ClassName, 'TUMLModelElement']));
end;

procedure TViolation._SetModelElement(const value: TUMLModelElement);
begin
  M_ModelElement.BoldObject := value;
end;

procedure TViolationList.Add(NewObject: TViolation);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TViolationList.IndexOf(anObject: TViolation): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TViolationList.Includes(anObject: TViolation) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TViolationList.AddNew: TViolation;
begin
  result := TViolation(InternalAddNew);
end;

procedure TViolationList.Insert(index: Integer; NewObject: TViolation);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TViolationList.GetBoldObject(index: Integer): TViolation;
begin
  result := TViolation(GetElement(index));
end;

procedure TViolationList.SetBoldObject(index: Integer; NewObject: TViolation);
begin;
  SetElement(index, NewObject);
end;

{ TUMLModelElement }

function TUMLModelElement._Get_M_name: TBAString;
begin
  assert(ValidateMember('TUMLModelElement', 'name', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TUMLModelElement._Getname: String;
begin
  Result := M_name.AsString;
end;

procedure TUMLModelElement._Setname(const NewValue: String);
begin
  M_name.AsString := NewValue;
end;

function TUMLModelElement._Get_M_visibility: TBAVisibilityKind;
begin
  assert(ValidateMember('TUMLModelElement', 'visibility', 1, TBAVisibilityKind));
  Result := TBAVisibilityKind(BoldMembers[1]);
end;

function TUMLModelElement._Getvisibility: TVisibilityKind;
begin
  Result := M_visibility.AsVisibilityKind;
end;

procedure TUMLModelElement._Setvisibility(const NewValue: TVisibilityKind);
begin
  M_visibility.AsVisibilityKind := NewValue;
end;

function TUMLModelElement._Get_M_isSpecification: TBABoolean;
begin
  assert(ValidateMember('TUMLModelElement', 'isSpecification', 2, TBABoolean));
  Result := TBABoolean(BoldMembers[2]);
end;

function TUMLModelElement._GetisSpecification: boolean;
begin
  Result := M_isSpecification.AsBoolean;
end;

procedure TUMLModelElement._SetisSpecification(const NewValue: boolean);
begin
  M_isSpecification.AsBoolean := NewValue;
end;

function TUMLModelElement._Get_M_qualifiedName: TBAString;
begin
  assert(ValidateMember('TUMLModelElement', 'qualifiedName', 3, TBAString));
  Result := TBAString(BoldMembers[3]);
end;

function TUMLModelElement._GetqualifiedName: String;
begin
  Result := M_qualifiedName.AsString;
end;

function TUMLModelElement._Get_M_stereotypeName: TBAString;
begin
  assert(ValidateMember('TUMLModelElement', 'stereotypeName', 4, TBAString));
  Result := TBAString(BoldMembers[4]);
end;

function TUMLModelElement._GetstereotypeName: String;
begin
  Result := M_stereotypeName.AsString;
end;

procedure TUMLModelElement._SetstereotypeName(const NewValue: String);
begin
  M_stereotypeName.AsString := NewValue;
end;

function TUMLModelElement._Get_M_documentation: TBAString;
begin
  assert(ValidateMember('TUMLModelElement', 'documentation', 5, TBAString));
  Result := TBAString(BoldMembers[5]);
end;

function TUMLModelElement._Getdocumentation: String;
begin
  Result := M_documentation.AsString;
end;

function TUMLModelElement._Get_M_derived: TBABoolean;
begin
  assert(ValidateMember('TUMLModelElement', 'derived', 6, TBABoolean));
  Result := TBABoolean(BoldMembers[6]);
end;

function TUMLModelElement._Getderived: boolean;
begin
  Result := M_derived.AsBoolean;
end;

procedure TUMLModelElement._Setderived(const NewValue: boolean);
begin
  M_derived.AsBoolean := NewValue;
end;

function TUMLModelElement._Getbehavior: TUMLStateMachineList;
begin
  assert(ValidateMember('TUMLModelElement', 'behavior', 7, TUMLStateMachineList));
  Result := TUMLStateMachineList(BoldMembers[7]);
end;

function TUMLModelElement._Getcollaboration: TUMLCollaborationList;
begin
  assert(ValidateMember('TUMLModelElement', 'collaboration', 8, TUMLCollaborationList));
  Result := TUMLCollaborationList(BoldMembers[8]);
end;

function TUMLModelElement._GetcollaborationcollaborationconstrainingElement: TcollaborationconstrainingElementList;
begin
  assert(ValidateMember('TUMLModelElement', 'collaborationcollaborationconstrainingElement', 9, TcollaborationconstrainingElementList));
  Result := TcollaborationconstrainingElementList(BoldMembers[9]);
end;

function TUMLModelElement._GetclassifierRole: TUMLClassifierRoleList;
begin
  assert(ValidateMember('TUMLModelElement', 'classifierRole', 10, TUMLClassifierRoleList));
  Result := TUMLClassifierRoleList(BoldMembers[10]);
end;

function TUMLModelElement._GetclassifierRoleclassifierRoleavailableContents: TclassifierRoleavailableContentsList;
begin
  assert(ValidateMember('TUMLModelElement', 'classifierRoleclassifierRoleavailableContents', 11, TclassifierRoleavailableContentsList));
  Result := TclassifierRoleavailableContentsList(BoldMembers[11]);
end;

function TUMLModelElement._Getpartition: TUMLPartitionList;
begin
  assert(ValidateMember('TUMLModelElement', 'partition', 12, TUMLPartitionList));
  Result := TUMLPartitionList(BoldMembers[12]);
end;

function TUMLModelElement._Getpartitioncontentspartition: TcontentspartitionList;
begin
  assert(ValidateMember('TUMLModelElement', 'partitioncontentspartition', 13, TcontentspartitionList));
  Result := TcontentspartitionList(BoldMembers[13]);
end;

function TUMLModelElement._Getpresentation: TUMLPresentationElementList;
begin
  assert(ValidateMember('TUMLModelElement', 'presentation', 14, TUMLPresentationElementList));
  Result := TUMLPresentationElementList(BoldMembers[14]);
end;

function TUMLModelElement._Getpresentationsubject: TpresentationsubjectList;
begin
  assert(ValidateMember('TUMLModelElement', 'presentationsubject', 15, TpresentationsubjectList));
  Result := TpresentationsubjectList(BoldMembers[15]);
end;

function TUMLModelElement._GetsourceFlow: TUMLFlowList;
begin
  assert(ValidateMember('TUMLModelElement', 'sourceFlow', 16, TUMLFlowList));
  Result := TUMLFlowList(BoldMembers[16]);
end;

function TUMLModelElement._GetsourceFlowsourceFlowsource: TsourceFlowsourceList;
begin
  assert(ValidateMember('TUMLModelElement', 'sourceFlowsourceFlowsource', 17, TsourceFlowsourceList));
  Result := TsourceFlowsourceList(BoldMembers[17]);
end;

function TUMLModelElement._GettargetFlow: TUMLFlowList;
begin
  assert(ValidateMember('TUMLModelElement', 'targetFlow', 18, TUMLFlowList));
  Result := TUMLFlowList(BoldMembers[18]);
end;

function TUMLModelElement._GettargetFlowtargetFlowtarget: TtargetFlowtargetList;
begin
  assert(ValidateMember('TUMLModelElement', 'targetFlowtargetFlowtarget', 19, TtargetFlowtargetList));
  Result := TtargetFlowtargetList(BoldMembers[19]);
end;

function TUMLModelElement._Get_M_binding: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLModelElement', 'binding', 20, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[20]);
end;

function TUMLModelElement._Getbinding: TUMLBinding;
begin
  Result := TUMLBinding(M_binding.BoldObject);
  assert(not assigned(Result) or (Result is TUMLBinding), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'binding', Result.ClassName, 'TUMLBinding']));
end;

procedure TUMLModelElement._Setbinding(const value: TUMLBinding);
begin
  M_binding.BoldObject := value;
end;

function TUMLModelElement._GetsupplierDependency: TUMLDependencyList;
begin
  assert(ValidateMember('TUMLModelElement', 'supplierDependency', 21, TUMLDependencyList));
  Result := TUMLDependencyList(BoldMembers[21]);
end;

function TUMLModelElement._GetsupplierDependencysuppliersupplierDependency: TsuppliersupplierDependencyList;
begin
  assert(ValidateMember('TUMLModelElement', 'supplierDependencysuppliersupplierDependency', 22, TsuppliersupplierDependencyList));
  Result := TsuppliersupplierDependencyList(BoldMembers[22]);
end;

function TUMLModelElement._Getconstraint: TUMLConstraintList;
begin
  assert(ValidateMember('TUMLModelElement', 'constraint', 23, TUMLConstraintList));
  Result := TUMLConstraintList(BoldMembers[23]);
end;

function TUMLModelElement._GetconstraintconstrainedElementconstraint: TconstrainedElementconstraintList;
begin
  assert(ValidateMember('TUMLModelElement', 'constraintconstrainedElementconstraint', 24, TconstrainedElementconstraintList));
  Result := TconstrainedElementconstraintList(BoldMembers[24]);
end;

function TUMLModelElement._GetclientDependency: TUMLDependencyList;
begin
  assert(ValidateMember('TUMLModelElement', 'clientDependency', 25, TUMLDependencyList));
  Result := TUMLDependencyList(BoldMembers[25]);
end;

function TUMLModelElement._GetclientDependencyclientclientDependency: TclientclientDependencyList;
begin
  assert(ValidateMember('TUMLModelElement', 'clientDependencyclientclientDependency', 26, TclientclientDependencyList));
  Result := TclientclientDependencyList(BoldMembers[26]);
end;

function TUMLModelElement._GettemplateParameter2: TUMLTemplateParameterList;
begin
  assert(ValidateMember('TUMLModelElement', 'templateParameter2', 27, TUMLTemplateParameterList));
  Result := TUMLTemplateParameterList(BoldMembers[27]);
end;

function TUMLModelElement._GettemplateParameter: TUMLTemplateParameterList;
begin
  assert(ValidateMember('TUMLModelElement', 'templateParameter', 28, TUMLTemplateParameterList));
  Result := TUMLTemplateParameterList(BoldMembers[28]);
end;

function TUMLModelElement._GetelementResidence: TUMLElementResidenceList;
begin
  assert(ValidateMember('TUMLModelElement', 'elementResidence', 29, TUMLElementResidenceList));
  Result := TUMLElementResidenceList(BoldMembers[29]);
end;

function TUMLModelElement._Getcomment: TUMLCommentList;
begin
  assert(ValidateMember('TUMLModelElement', 'comment', 30, TUMLCommentList));
  Result := TUMLCommentList(BoldMembers[30]);
end;

function TUMLModelElement._GetcommentcommentannotatedElement: TcommentannotatedElementList;
begin
  assert(ValidateMember('TUMLModelElement', 'commentcommentannotatedElement', 31, TcommentannotatedElementList));
  Result := TcommentannotatedElementList(BoldMembers[31]);
end;

function TUMLModelElement._Get_M_namespace_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLModelElement', 'namespace_', 33, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[33]);
end;

function TUMLModelElement._Getnamespace_: TUMLNamespace;
begin
  Result := TUMLNamespace(M_namespace_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLNamespace), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'namespace_', Result.ClassName, 'TUMLNamespace']));
end;

procedure TUMLModelElement._Setnamespace_(const value: TUMLNamespace);
begin
  M_namespace_.BoldObject := value;
end;

function TUMLModelElement._GettaggedValue: TUMLTaggedValueList;
begin
  assert(ValidateMember('TUMLModelElement', 'taggedValue', 34, TUMLTaggedValueList));
  Result := TUMLTaggedValueList(BoldMembers[34]);
end;

function TUMLModelElement._Get_Q_taggedValue(tag: String): TUMLTaggedValue;
var
  TempResult: TBoldObject;
  TempList: TBoldMemberList;
  Q_tag: TBAString;
begin
  TempList := TBoldMemberList.Create;
  TempList.CloneMembers := false;
  Q_tag := TBAString.Create;
  try
    Q_tag.AsString := tag;
    TempList.add(Q_tag);
    TempResult := M_taggedValue.GetByIndex(TempList);
    assert(not assigned(TempResult) or (TempResult is TUMLTaggedValue), 'Illegal object in multilink');
    result := TUMLTaggedValue(TempResult);
  finally
    TempList.Free;
    Q_tag.Free;
  end;
end;

function TUMLModelElement._Get_M_stereotype: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLModelElement', 'stereotype', 35, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[35]);
end;

function TUMLModelElement._Getstereotype: TUMLStereotype;
begin
  Result := TUMLStereotype(M_stereotype.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStereotype), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'stereotype', Result.ClassName, 'TUMLStereotype']));
end;

procedure TUMLModelElement._Setstereotype(const value: TUMLStereotype);
begin
  M_stereotype.BoldObject := value;
end;

function TUMLModelElement._Get_M_model: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLModelElement', 'model', 36, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[36]);
end;

function TUMLModelElement._Getmodel: TUMLModel;
begin
  Result := TUMLModel(M_model.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModel), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'model', Result.ClassName, 'TUMLModel']));
end;

procedure TUMLModelElement._Setmodel(const value: TUMLModel);
begin
  M_model.BoldObject := value;
end;

function TUMLModelElement._Get_M_qualifyingOwner: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLModelElement', 'qualifyingOwner', 38, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[38]);
end;

function TUMLModelElement._GetqualifyingOwner: TUMLModelElement;
begin
  Result := TUMLModelElement(M_qualifyingOwner.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'qualifyingOwner', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLModelElement._SetqualifyingOwner(const value: TUMLModelElement);
begin
  M_qualifyingOwner.BoldObject := value;
end;

function TUMLModelElement._GetelementImport: TUMLElementImportList;
begin
  assert(ValidateMember('TUMLModelElement', 'elementImport', 39, TUMLElementImportList));
  Result := TUMLElementImportList(BoldMembers[39]);
end;

procedure TUMLModelElementList.Add(NewObject: TUMLModelElement);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLModelElementList.IndexOf(anObject: TUMLModelElement): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLModelElementList.Includes(anObject: TUMLModelElement) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLModelElementList.AddNew: TUMLModelElement;
begin
  result := TUMLModelElement(InternalAddNew);
end;

procedure TUMLModelElementList.Insert(index: Integer; NewObject: TUMLModelElement);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLModelElementList.GetBoldObject(index: Integer): TUMLModelElement;
begin
  result := TUMLModelElement(GetElement(index));
end;

procedure TUMLModelElementList.SetBoldObject(index: Integer; NewObject: TUMLModelElement);
begin;
  SetElement(index, NewObject);
end;

function TUMLModelElement.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    4: result := _stereotypeName_DeriveAndSubscribe;
    5: result := _documentation_DeriveAndSubscribe;
    6: result := _derived_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLModelElement.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    4: result := _stereotypeName_ReverseDerive;
    6: result := _derived_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLAction }

function TUMLAction._Get_M_recurrence: TBAString;
begin
  assert(ValidateMember('TUMLAction', 'recurrence', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLAction._Getrecurrence: String;
begin
  Result := M_recurrence.AsString;
end;

procedure TUMLAction._Setrecurrence(const NewValue: String);
begin
  M_recurrence.AsString := NewValue;
end;

function TUMLAction._Get_M_target: TBAString;
begin
  assert(ValidateMember('TUMLAction', 'target', 41, TBAString));
  Result := TBAString(BoldMembers[41]);
end;

function TUMLAction._Gettarget: String;
begin
  Result := M_target.AsString;
end;

procedure TUMLAction._Settarget(const NewValue: String);
begin
  M_target.AsString := NewValue;
end;

function TUMLAction._Get_M_isAsynchronous: TBABoolean;
begin
  assert(ValidateMember('TUMLAction', 'isAsynchronous', 42, TBABoolean));
  Result := TBABoolean(BoldMembers[42]);
end;

function TUMLAction._GetisAsynchronous: boolean;
begin
  Result := M_isAsynchronous.AsBoolean;
end;

procedure TUMLAction._SetisAsynchronous(const NewValue: boolean);
begin
  M_isAsynchronous.AsBoolean := NewValue;
end;

function TUMLAction._Get_M_script: TBAString;
begin
  assert(ValidateMember('TUMLAction', 'script', 43, TBAString));
  Result := TBAString(BoldMembers[43]);
end;

function TUMLAction._Getscript: String;
begin
  Result := M_script.AsString;
end;

procedure TUMLAction._Setscript(const NewValue: String);
begin
  M_script.AsString := NewValue;
end;

function TUMLAction._Get_M_transition: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAction', 'transition', 45, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[45]);
end;

function TUMLAction._Gettransition: TUMLTransition;
begin
  Result := TUMLTransition(M_transition.BoldObject);
  assert(not assigned(Result) or (Result is TUMLTransition), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'transition', Result.ClassName, 'TUMLTransition']));
end;

procedure TUMLAction._Settransition(const value: TUMLTransition);
begin
  M_transition.BoldObject := value;
end;

function TUMLAction._GetactualArgument: TUMLArgumentList;
begin
  assert(ValidateMember('TUMLAction', 'actualArgument', 48, TUMLArgumentList));
  Result := TUMLArgumentList(BoldMembers[48]);
end;

function TUMLAction._Getstimulus: TUMLStimulusList;
begin
  assert(ValidateMember('TUMLAction', 'stimulus', 49, TUMLStimulusList));
  Result := TUMLStimulusList(BoldMembers[49]);
end;

function TUMLAction._Get_M_actionSequence: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAction', 'actionSequence', 50, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[50]);
end;

function TUMLAction._GetactionSequence: TUMLActionSequence;
begin
  Result := TUMLActionSequence(M_actionSequence.BoldObject);
  assert(not assigned(Result) or (Result is TUMLActionSequence), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'actionSequence', Result.ClassName, 'TUMLActionSequence']));
end;

procedure TUMLAction._SetactionSequence(const value: TUMLActionSequence);
begin
  M_actionSequence.BoldObject := value;
end;

function TUMLAction._Getmessage_: TUMLMessageList;
begin
  assert(ValidateMember('TUMLAction', 'message_', 51, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[51]);
end;

procedure TUMLActionList.Add(NewObject: TUMLAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLActionList.IndexOf(anObject: TUMLAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLActionList.Includes(anObject: TUMLAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLActionList.AddNew: TUMLAction;
begin
  result := TUMLAction(InternalAddNew);
end;

procedure TUMLActionList.Insert(index: Integer; NewObject: TUMLAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLActionList.GetBoldObject(index: Integer): TUMLAction;
begin
  result := TUMLAction(GetElement(index));
end;

procedure TUMLActionList.SetBoldObject(index: Integer; NewObject: TUMLAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLArgument }

function TUMLArgument._Get_M_value: TBAString;
begin
  assert(ValidateMember('TUMLArgument', 'value', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLArgument._Getvalue: String;
begin
  Result := M_value.AsString;
end;

procedure TUMLArgument._Setvalue(const NewValue: String);
begin
  M_value.AsString := NewValue;
end;

function TUMLArgument._Get_M_action: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLArgument', 'action', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLArgument._Getaction: TUMLAction;
begin
  Result := TUMLAction(M_action.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'action', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLArgument._Setaction(const value: TUMLAction);
begin
  M_action.BoldObject := value;
end;

procedure TUMLArgumentList.Add(NewObject: TUMLArgument);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLArgumentList.IndexOf(anObject: TUMLArgument): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLArgumentList.Includes(anObject: TUMLArgument) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLArgumentList.AddNew: TUMLArgument;
begin
  result := TUMLArgument(InternalAddNew);
end;

procedure TUMLArgumentList.Insert(index: Integer; NewObject: TUMLArgument);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLArgumentList.GetBoldObject(index: Integer): TUMLArgument;
begin
  result := TUMLArgument(GetElement(index));
end;

procedure TUMLArgumentList.SetBoldObject(index: Integer; NewObject: TUMLArgument);
begin;
  SetElement(index, NewObject);
end;

{ TUMLAssociationEnd }

function TUMLAssociationEnd._Get_M_isNavigable: TBABoolean;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'isNavigable', 40, TBABoolean));
  Result := TBABoolean(BoldMembers[40]);
end;

function TUMLAssociationEnd._GetisNavigable: boolean;
begin
  Result := M_isNavigable.AsBoolean;
end;

procedure TUMLAssociationEnd._SetisNavigable(const NewValue: boolean);
begin
  M_isNavigable.AsBoolean := NewValue;
end;

function TUMLAssociationEnd._Get_M_ordering: TBAOrderingKind;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'ordering', 41, TBAOrderingKind));
  Result := TBAOrderingKind(BoldMembers[41]);
end;

function TUMLAssociationEnd._Getordering: TOrderingKind;
begin
  Result := M_ordering.AsOrderingKind;
end;

procedure TUMLAssociationEnd._Setordering(const NewValue: TOrderingKind);
begin
  M_ordering.AsOrderingKind := NewValue;
end;

function TUMLAssociationEnd._Get_M_aggregation: TBAAggregationKind;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'aggregation', 42, TBAAggregationKind));
  Result := TBAAggregationKind(BoldMembers[42]);
end;

function TUMLAssociationEnd._Getaggregation: TAggregationKind;
begin
  Result := M_aggregation.AsAggregationKind;
end;

procedure TUMLAssociationEnd._Setaggregation(const NewValue: TAggregationKind);
begin
  M_aggregation.AsAggregationKind := NewValue;
end;

function TUMLAssociationEnd._Get_M_targetScope: TBAScopeKind;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'targetScope', 43, TBAScopeKind));
  Result := TBAScopeKind(BoldMembers[43]);
end;

function TUMLAssociationEnd._GettargetScope: TScopeKind;
begin
  Result := M_targetScope.AsScopeKind;
end;

procedure TUMLAssociationEnd._SettargetScope(const NewValue: TScopeKind);
begin
  M_targetScope.AsScopeKind := NewValue;
end;

function TUMLAssociationEnd._Get_M_multiplicity: TBAString;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'multiplicity', 44, TBAString));
  Result := TBAString(BoldMembers[44]);
end;

function TUMLAssociationEnd._Getmultiplicity: String;
begin
  Result := M_multiplicity.AsString;
end;

procedure TUMLAssociationEnd._Setmultiplicity(const NewValue: String);
begin
  M_multiplicity.AsString := NewValue;
end;

function TUMLAssociationEnd._Get_M_changeability: TBAChangeableKind;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'changeability', 45, TBAChangeableKind));
  Result := TBAChangeableKind(BoldMembers[45]);
end;

function TUMLAssociationEnd._Getchangeability: TChangeableKind;
begin
  Result := M_changeability.AsChangeableKind;
end;

procedure TUMLAssociationEnd._Setchangeability(const NewValue: TChangeableKind);
begin
  M_changeability.AsChangeableKind := NewValue;
end;

function TUMLAssociationEnd._Get_M_multi: TBABoolean;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'multi', 46, TBABoolean));
  Result := TBABoolean(BoldMembers[46]);
end;

function TUMLAssociationEnd._Getmulti: boolean;
begin
  Result := M_multi.AsBoolean;
end;

function TUMLAssociationEnd._Get_M_mandatory: TBABoolean;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'mandatory', 47, TBABoolean));
  Result := TBABoolean(BoldMembers[47]);
end;

function TUMLAssociationEnd._Getmandatory: boolean;
begin
  Result := M_mandatory.AsBoolean;
end;

function TUMLAssociationEnd._Get_M_isOrdered: TBABoolean;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'isOrdered', 48, TBABoolean));
  Result := TBABoolean(BoldMembers[48]);
end;

function TUMLAssociationEnd._GetisOrdered: boolean;
begin
  Result := M_isOrdered.AsBoolean;
end;

procedure TUMLAssociationEnd._SetisOrdered(const NewValue: boolean);
begin
  M_isOrdered.AsBoolean := NewValue;
end;

function TUMLAssociationEnd._GetlinkEnd: TUMLLinkEndList;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'linkEnd', 49, TUMLLinkEndList));
  Result := TUMLLinkEndList(BoldMembers[49]);
end;

function TUMLAssociationEnd._GetassociationEndRole: TUMLAssociationEndRoleList;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'associationEndRole', 50, TUMLAssociationEndRoleList));
  Result := TUMLAssociationEndRoleList(BoldMembers[50]);
end;

function TUMLAssociationEnd._Getqualifier: TUMLAttributeList;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'qualifier', 51, TUMLAttributeList));
  Result := TUMLAttributeList(BoldMembers[51]);
end;

function TUMLAssociationEnd._Getspecification: TUMLClassifierList;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'specification', 52, TUMLClassifierList));
  Result := TUMLClassifierList(BoldMembers[52]);
end;

function TUMLAssociationEnd._Getparticipantspecification: TparticipantspecificationList;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'participantspecification', 53, TparticipantspecificationList));
  Result := TparticipantspecificationList(BoldMembers[53]);
end;

function TUMLAssociationEnd._Get_M_type_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'type_', 54, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[54]);
end;

function TUMLAssociationEnd._Gettype_: TUMLClassifier;
begin
  Result := TUMLClassifier(M_type_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'type_', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLAssociationEnd._Settype_(const value: TUMLClassifier);
begin
  M_type_.BoldObject := value;
end;

function TUMLAssociationEnd._Get_M_otherEnd: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'otherEnd', 55, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[55]);
end;

function TUMLAssociationEnd._GetotherEnd: TUMLAssociationEnd;
begin
  Result := TUMLAssociationEnd(M_otherEnd.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationEnd), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'otherEnd', Result.ClassName, 'TUMLAssociationEnd']));
end;

procedure TUMLAssociationEnd._SetotherEnd(const value: TUMLAssociationEnd);
begin
  M_otherEnd.BoldObject := value;
end;

function TUMLAssociationEnd._Get_M_association: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAssociationEnd', 'association', 56, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[56]);
end;

function TUMLAssociationEnd._Getassociation: TUMLAssociation;
begin
  Result := TUMLAssociation(M_association.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'association', Result.ClassName, 'TUMLAssociation']));
end;

procedure TUMLAssociationEnd._Setassociation(const value: TUMLAssociation);
begin
  M_association.BoldObject := value;
end;

procedure TUMLAssociationEndList.Add(NewObject: TUMLAssociationEnd);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAssociationEndList.IndexOf(anObject: TUMLAssociationEnd): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAssociationEndList.Includes(anObject: TUMLAssociationEnd) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAssociationEndList.AddNew: TUMLAssociationEnd;
begin
  result := TUMLAssociationEnd(InternalAddNew);
end;

procedure TUMLAssociationEndList.Insert(index: Integer; NewObject: TUMLAssociationEnd);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAssociationEndList.GetBoldObject(index: Integer): TUMLAssociationEnd;
begin
  result := TUMLAssociationEnd(GetElement(index));
end;

procedure TUMLAssociationEndList.SetBoldObject(index: Integer; NewObject: TUMLAssociationEnd);
begin;
  SetElement(index, NewObject);
end;

function TUMLAssociationEnd.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    46: result := _multi_DeriveAndSubscribe;
    47: result := _mandatory_DeriveAndSubscribe;
    48: result := _isOrdered_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLAssociationEnd.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    48: result := _isOrdered_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLAttributeLink }

function TUMLAttributeLink._Get_M_value: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAttributeLink', 'value', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLAttributeLink._Getvalue: TUMLInstance;
begin
  Result := TUMLInstance(M_value.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'value', Result.ClassName, 'TUMLInstance']));
end;

procedure TUMLAttributeLink._Setvalue(const value: TUMLInstance);
begin
  M_value.BoldObject := value;
end;

function TUMLAttributeLink._Get_M_attribute: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAttributeLink', 'attribute', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLAttributeLink._Getattribute: TUMLAttribute;
begin
  Result := TUMLAttribute(M_attribute.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAttribute), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'attribute', Result.ClassName, 'TUMLAttribute']));
end;

procedure TUMLAttributeLink._Setattribute(const value: TUMLAttribute);
begin
  M_attribute.BoldObject := value;
end;

function TUMLAttributeLink._Get_M_instance: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAttributeLink', 'instance', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLAttributeLink._Getinstance: TUMLInstance;
begin
  Result := TUMLInstance(M_instance.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'instance', Result.ClassName, 'TUMLInstance']));
end;

procedure TUMLAttributeLink._Setinstance(const value: TUMLInstance);
begin
  M_instance.BoldObject := value;
end;

function TUMLAttributeLink._Get_M_linkEnd: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAttributeLink', 'linkEnd', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLAttributeLink._GetlinkEnd: TUMLLinkEnd;
begin
  Result := TUMLLinkEnd(M_linkEnd.BoldObject);
  assert(not assigned(Result) or (Result is TUMLLinkEnd), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'linkEnd', Result.ClassName, 'TUMLLinkEnd']));
end;

procedure TUMLAttributeLink._SetlinkEnd(const value: TUMLLinkEnd);
begin
  M_linkEnd.BoldObject := value;
end;

procedure TUMLAttributeLinkList.Add(NewObject: TUMLAttributeLink);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAttributeLinkList.IndexOf(anObject: TUMLAttributeLink): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAttributeLinkList.Includes(anObject: TUMLAttributeLink) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAttributeLinkList.AddNew: TUMLAttributeLink;
begin
  result := TUMLAttributeLink(InternalAddNew);
end;

procedure TUMLAttributeLinkList.Insert(index: Integer; NewObject: TUMLAttributeLink);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAttributeLinkList.GetBoldObject(index: Integer): TUMLAttributeLink;
begin
  result := TUMLAttributeLink(GetElement(index));
end;

procedure TUMLAttributeLinkList.SetBoldObject(index: Integer; NewObject: TUMLAttributeLink);
begin;
  SetElement(index, NewObject);
end;

{ TUMLComment }

function TUMLComment._GetannotatedElement: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLComment', 'annotatedElement', 40, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[40]);
end;

function TUMLComment._GetannotatedElementcommentannotatedElement: TcommentannotatedElementList;
begin
  assert(ValidateMember('TUMLComment', 'annotatedElementcommentannotatedElement', 41, TcommentannotatedElementList));
  Result := TcommentannotatedElementList(BoldMembers[41]);
end;

procedure TUMLCommentList.Add(NewObject: TUMLComment);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCommentList.IndexOf(anObject: TUMLComment): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCommentList.Includes(anObject: TUMLComment) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCommentList.AddNew: TUMLComment;
begin
  result := TUMLComment(InternalAddNew);
end;

procedure TUMLCommentList.Insert(index: Integer; NewObject: TUMLComment);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCommentList.GetBoldObject(index: Integer): TUMLComment;
begin
  result := TUMLComment(GetElement(index));
end;

procedure TUMLCommentList.SetBoldObject(index: Integer; NewObject: TUMLComment);
begin;
  SetElement(index, NewObject);
end;

{ TUMLConstraint }

function TUMLConstraint._Get_M_body: TBAString;
begin
  assert(ValidateMember('TUMLConstraint', 'body', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLConstraint._Getbody: String;
begin
  Result := M_body.AsString;
end;

procedure TUMLConstraint._Setbody(const NewValue: String);
begin
  M_body.AsString := NewValue;
end;

function TUMLConstraint._GetconstrainedElement: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLConstraint', 'constrainedElement', 41, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[41]);
end;

function TUMLConstraint._GetconstrainedElementconstrainedElementconstraint: TconstrainedElementconstraintList;
begin
  assert(ValidateMember('TUMLConstraint', 'constrainedElementconstrainedElementconstraint', 42, TconstrainedElementconstraintList));
  Result := TconstrainedElementconstraintList(BoldMembers[42]);
end;

function TUMLConstraint._Get_M_constrainedElement2: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLConstraint', 'constrainedElement2', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLConstraint._GetconstrainedElement2: TUMLStereotype;
begin
  Result := TUMLStereotype(M_constrainedElement2.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStereotype), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'constrainedElement2', Result.ClassName, 'TUMLStereotype']));
end;

procedure TUMLConstraint._SetconstrainedElement2(const value: TUMLStereotype);
begin
  M_constrainedElement2.BoldObject := value;
end;

procedure TUMLConstraintList.Add(NewObject: TUMLConstraint);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLConstraintList.IndexOf(anObject: TUMLConstraint): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLConstraintList.Includes(anObject: TUMLConstraint) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLConstraintList.AddNew: TUMLConstraint;
begin
  result := TUMLConstraint(InternalAddNew);
end;

procedure TUMLConstraintList.Insert(index: Integer; NewObject: TUMLConstraint);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLConstraintList.GetBoldObject(index: Integer): TUMLConstraint;
begin
  result := TUMLConstraint(GetElement(index));
end;

procedure TUMLConstraintList.SetBoldObject(index: Integer; NewObject: TUMLConstraint);
begin;
  SetElement(index, NewObject);
end;

{ TUMLEvent }

function TUMLEvent._Gettransition: TUMLTransitionList;
begin
  assert(ValidateMember('TUMLEvent', 'transition', 40, TUMLTransitionList));
  Result := TUMLTransitionList(BoldMembers[40]);
end;

function TUMLEvent._Getparameter: TUMLParameterList;
begin
  assert(ValidateMember('TUMLEvent', 'parameter', 41, TUMLParameterList));
  Result := TUMLParameterList(BoldMembers[41]);
end;

function TUMLEvent._Getstate: TUMLStateList;
begin
  assert(ValidateMember('TUMLEvent', 'state', 42, TUMLStateList));
  Result := TUMLStateList(BoldMembers[42]);
end;

function TUMLEvent._GetstatedeferrableEvent: TstatedeferrableEventList;
begin
  assert(ValidateMember('TUMLEvent', 'statedeferrableEvent', 43, TstatedeferrableEventList));
  Result := TstatedeferrableEventList(BoldMembers[43]);
end;

procedure TUMLEventList.Add(NewObject: TUMLEvent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLEventList.IndexOf(anObject: TUMLEvent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLEventList.Includes(anObject: TUMLEvent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLEventList.AddNew: TUMLEvent;
begin
  result := TUMLEvent(InternalAddNew);
end;

procedure TUMLEventList.Insert(index: Integer; NewObject: TUMLEvent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLEventList.GetBoldObject(index: Integer): TUMLEvent;
begin
  result := TUMLEvent(GetElement(index));
end;

procedure TUMLEventList.SetBoldObject(index: Integer; NewObject: TUMLEvent);
begin;
  SetElement(index, NewObject);
end;

{ TUMLExtensionPoint }

function TUMLExtensionPoint._Get_M_location: TBAString;
begin
  assert(ValidateMember('TUMLExtensionPoint', 'location', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLExtensionPoint._Getlocation: String;
begin
  Result := M_location.AsString;
end;

procedure TUMLExtensionPoint._Setlocation(const NewValue: String);
begin
  M_location.AsString := NewValue;
end;

function TUMLExtensionPoint._Get_M_useCase: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLExtensionPoint', 'useCase', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLExtensionPoint._GetuseCase: TUMLUseCase;
begin
  Result := TUMLUseCase(M_useCase.BoldObject);
  assert(not assigned(Result) or (Result is TUMLUseCase), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'useCase', Result.ClassName, 'TUMLUseCase']));
end;

procedure TUMLExtensionPoint._SetuseCase(const value: TUMLUseCase);
begin
  M_useCase.BoldObject := value;
end;

function TUMLExtensionPoint._Getextend: TUMLExtendList;
begin
  assert(ValidateMember('TUMLExtensionPoint', 'extend', 42, TUMLExtendList));
  Result := TUMLExtendList(BoldMembers[42]);
end;

function TUMLExtensionPoint._GetextensionPointextend: TextensionPointextendList;
begin
  assert(ValidateMember('TUMLExtensionPoint', 'extensionPointextend', 43, TextensionPointextendList));
  Result := TextensionPointextendList(BoldMembers[43]);
end;

procedure TUMLExtensionPointList.Add(NewObject: TUMLExtensionPoint);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLExtensionPointList.IndexOf(anObject: TUMLExtensionPoint): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLExtensionPointList.Includes(anObject: TUMLExtensionPoint) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLExtensionPointList.AddNew: TUMLExtensionPoint;
begin
  result := TUMLExtensionPoint(InternalAddNew);
end;

procedure TUMLExtensionPointList.Insert(index: Integer; NewObject: TUMLExtensionPoint);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLExtensionPointList.GetBoldObject(index: Integer): TUMLExtensionPoint;
begin
  result := TUMLExtensionPoint(GetElement(index));
end;

procedure TUMLExtensionPointList.SetBoldObject(index: Integer; NewObject: TUMLExtensionPoint);
begin;
  SetElement(index, NewObject);
end;

{ TUMLFeature }

function TUMLFeature._Get_M_ownerScope: TBAScopeKind;
begin
  assert(ValidateMember('TUMLFeature', 'ownerScope', 40, TBAScopeKind));
  Result := TBAScopeKind(BoldMembers[40]);
end;

function TUMLFeature._GetownerScope: TScopeKind;
begin
  Result := M_ownerScope.AsScopeKind;
end;

procedure TUMLFeature._SetownerScope(const NewValue: TScopeKind);
begin
  M_ownerScope.AsScopeKind := NewValue;
end;

function TUMLFeature._GetclassifierRole_: TUMLClassifierRoleList;
begin
  assert(ValidateMember('TUMLFeature', 'classifierRole_', 41, TUMLClassifierRoleList));
  Result := TUMLClassifierRoleList(BoldMembers[41]);
end;

function TUMLFeature._GetclassifierRole_availableFeature: TclassifierRole_availableFeatureList;
begin
  assert(ValidateMember('TUMLFeature', 'classifierRole_availableFeature', 42, TclassifierRole_availableFeatureList));
  Result := TclassifierRole_availableFeatureList(BoldMembers[42]);
end;

function TUMLFeature._Get_M_owner: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLFeature', 'owner', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLFeature._Getowner: TUMLClassifier;
begin
  Result := TUMLClassifier(M_owner.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'owner', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLFeature._Setowner(const value: TUMLClassifier);
begin
  M_owner.BoldObject := value;
end;

procedure TUMLFeatureList.Add(NewObject: TUMLFeature);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLFeatureList.IndexOf(anObject: TUMLFeature): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLFeatureList.Includes(anObject: TUMLFeature) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLFeatureList.AddNew: TUMLFeature;
begin
  result := TUMLFeature(InternalAddNew);
end;

procedure TUMLFeatureList.Insert(index: Integer; NewObject: TUMLFeature);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLFeatureList.GetBoldObject(index: Integer): TUMLFeature;
begin
  result := TUMLFeature(GetElement(index));
end;

procedure TUMLFeatureList.SetBoldObject(index: Integer; NewObject: TUMLFeature);
begin;
  SetElement(index, NewObject);
end;

{ TUMLGuard }

function TUMLGuard._Get_M_expression: TBAString;
begin
  assert(ValidateMember('TUMLGuard', 'expression', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLGuard._Getexpression: String;
begin
  Result := M_expression.AsString;
end;

procedure TUMLGuard._Setexpression(const NewValue: String);
begin
  M_expression.AsString := NewValue;
end;

function TUMLGuard._Get_M_transition: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLGuard', 'transition', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLGuard._Gettransition: TUMLTransition;
begin
  Result := TUMLTransition(M_transition.BoldObject);
  assert(not assigned(Result) or (Result is TUMLTransition), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'transition', Result.ClassName, 'TUMLTransition']));
end;

procedure TUMLGuard._Settransition(const value: TUMLTransition);
begin
  M_transition.BoldObject := value;
end;

procedure TUMLGuardList.Add(NewObject: TUMLGuard);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLGuardList.IndexOf(anObject: TUMLGuard): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLGuardList.Includes(anObject: TUMLGuard) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLGuardList.AddNew: TUMLGuard;
begin
  result := TUMLGuard(InternalAddNew);
end;

procedure TUMLGuardList.Insert(index: Integer; NewObject: TUMLGuard);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLGuardList.GetBoldObject(index: Integer): TUMLGuard;
begin
  result := TUMLGuard(GetElement(index));
end;

procedure TUMLGuardList.SetBoldObject(index: Integer; NewObject: TUMLGuard);
begin;
  SetElement(index, NewObject);
end;

{ TUMLInstance }

function TUMLInstance._GetattributeLink: TUMLAttributeLinkList;
begin
  assert(ValidateMember('TUMLInstance', 'attributeLink', 40, TUMLAttributeLinkList));
  Result := TUMLAttributeLinkList(BoldMembers[40]);
end;

function TUMLInstance._Getslot: TUMLAttributeLinkList;
begin
  assert(ValidateMember('TUMLInstance', 'slot', 41, TUMLAttributeLinkList));
  Result := TUMLAttributeLinkList(BoldMembers[41]);
end;

function TUMLInstance._Getstimulus1: TUMLStimulusList;
begin
  assert(ValidateMember('TUMLInstance', 'stimulus1', 42, TUMLStimulusList));
  Result := TUMLStimulusList(BoldMembers[42]);
end;

function TUMLInstance._Getargumentstimulus1: Targumentstimulus1List;
begin
  assert(ValidateMember('TUMLInstance', 'argumentstimulus1', 43, Targumentstimulus1List));
  Result := Targumentstimulus1List(BoldMembers[43]);
end;

function TUMLInstance._Getstimulus2: TUMLStimulusList;
begin
  assert(ValidateMember('TUMLInstance', 'stimulus2', 44, TUMLStimulusList));
  Result := TUMLStimulusList(BoldMembers[44]);
end;

function TUMLInstance._GetlinkEnd: TUMLLinkEndList;
begin
  assert(ValidateMember('TUMLInstance', 'linkEnd', 45, TUMLLinkEndList));
  Result := TUMLLinkEndList(BoldMembers[45]);
end;

function TUMLInstance._Getclassifier: TUMLClassifierList;
begin
  assert(ValidateMember('TUMLInstance', 'classifier', 46, TUMLClassifierList));
  Result := TUMLClassifierList(BoldMembers[46]);
end;

function TUMLInstance._Getinstanceclassifier: TinstanceclassifierList;
begin
  assert(ValidateMember('TUMLInstance', 'instanceclassifier', 47, TinstanceclassifierList));
  Result := TinstanceclassifierList(BoldMembers[47]);
end;

function TUMLInstance._Getstimulus3: TUMLStimulusList;
begin
  assert(ValidateMember('TUMLInstance', 'stimulus3', 48, TUMLStimulusList));
  Result := TUMLStimulusList(BoldMembers[48]);
end;

function TUMLInstance._Get_M_componentInstance: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLInstance', 'componentInstance', 49, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[49]);
end;

function TUMLInstance._GetcomponentInstance: TUMLComponentInstance;
begin
  Result := TUMLComponentInstance(M_componentInstance.BoldObject);
  assert(not assigned(Result) or (Result is TUMLComponentInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'componentInstance', Result.ClassName, 'TUMLComponentInstance']));
end;

procedure TUMLInstance._SetcomponentInstance(const value: TUMLComponentInstance);
begin
  M_componentInstance.BoldObject := value;
end;

procedure TUMLInstanceList.Add(NewObject: TUMLInstance);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLInstanceList.IndexOf(anObject: TUMLInstance): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLInstanceList.Includes(anObject: TUMLInstance) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLInstanceList.AddNew: TUMLInstance;
begin
  result := TUMLInstance(InternalAddNew);
end;

procedure TUMLInstanceList.Insert(index: Integer; NewObject: TUMLInstance);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLInstanceList.GetBoldObject(index: Integer): TUMLInstance;
begin
  result := TUMLInstance(GetElement(index));
end;

procedure TUMLInstanceList.SetBoldObject(index: Integer; NewObject: TUMLInstance);
begin;
  SetElement(index, NewObject);
end;

{ TUMLInteraction }

function TUMLInteraction._Get_M_context: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLInteraction', 'context', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLInteraction._Getcontext: TUMLCollaboration;
begin
  Result := TUMLCollaboration(M_context.BoldObject);
  assert(not assigned(Result) or (Result is TUMLCollaboration), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'context', Result.ClassName, 'TUMLCollaboration']));
end;

procedure TUMLInteraction._Setcontext(const value: TUMLCollaboration);
begin
  M_context.BoldObject := value;
end;

function TUMLInteraction._Getmessage_: TUMLMessageList;
begin
  assert(ValidateMember('TUMLInteraction', 'message_', 41, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[41]);
end;

procedure TUMLInteractionList.Add(NewObject: TUMLInteraction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLInteractionList.IndexOf(anObject: TUMLInteraction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLInteractionList.Includes(anObject: TUMLInteraction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLInteractionList.AddNew: TUMLInteraction;
begin
  result := TUMLInteraction(InternalAddNew);
end;

procedure TUMLInteractionList.Insert(index: Integer; NewObject: TUMLInteraction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLInteractionList.GetBoldObject(index: Integer): TUMLInteraction;
begin
  result := TUMLInteraction(GetElement(index));
end;

procedure TUMLInteractionList.SetBoldObject(index: Integer; NewObject: TUMLInteraction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLLink }

function TUMLLink._Get_M_association: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLLink', 'association', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLLink._Getassociation: TUMLAssociation;
begin
  Result := TUMLAssociation(M_association.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'association', Result.ClassName, 'TUMLAssociation']));
end;

procedure TUMLLink._Setassociation(const value: TUMLAssociation);
begin
  M_association.BoldObject := value;
end;

function TUMLLink._GetStimulus: TUMLStimulusList;
begin
  assert(ValidateMember('TUMLLink', 'Stimulus', 41, TUMLStimulusList));
  Result := TUMLStimulusList(BoldMembers[41]);
end;

function TUMLLink._Get_M_xobject: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLLink', 'xobject', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLLink._Getxobject: TUMLObject;
begin
  Result := TUMLObject(M_xobject.BoldObject);
  assert(not assigned(Result) or (Result is TUMLObject), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'xobject', Result.ClassName, 'TUMLObject']));
end;

procedure TUMLLink._Setxobject(const value: TUMLObject);
begin
  M_xobject.BoldObject := value;
end;

function TUMLLink._Getconnection: TUMLLinkEndList;
begin
  assert(ValidateMember('TUMLLink', 'connection', 43, TUMLLinkEndList));
  Result := TUMLLinkEndList(BoldMembers[43]);
end;

procedure TUMLLinkList.Add(NewObject: TUMLLink);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLLinkList.IndexOf(anObject: TUMLLink): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLLinkList.Includes(anObject: TUMLLink) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLLinkList.AddNew: TUMLLink;
begin
  result := TUMLLink(InternalAddNew);
end;

procedure TUMLLinkList.Insert(index: Integer; NewObject: TUMLLink);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLLinkList.GetBoldObject(index: Integer): TUMLLink;
begin
  result := TUMLLink(GetElement(index));
end;

procedure TUMLLinkList.SetBoldObject(index: Integer; NewObject: TUMLLink);
begin;
  SetElement(index, NewObject);
end;

{ TUMLLinkEnd }

function TUMLLinkEnd._GetqualifiedValue: TUMLAttributeLinkList;
begin
  assert(ValidateMember('TUMLLinkEnd', 'qualifiedValue', 40, TUMLAttributeLinkList));
  Result := TUMLAttributeLinkList(BoldMembers[40]);
end;

function TUMLLinkEnd._Get_M_instance: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLLinkEnd', 'instance', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLLinkEnd._Getinstance: TUMLInstance;
begin
  Result := TUMLInstance(M_instance.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'instance', Result.ClassName, 'TUMLInstance']));
end;

procedure TUMLLinkEnd._Setinstance(const value: TUMLInstance);
begin
  M_instance.BoldObject := value;
end;

function TUMLLinkEnd._Get_M_associationEnd: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLLinkEnd', 'associationEnd', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLLinkEnd._GetassociationEnd: TUMLAssociationEnd;
begin
  Result := TUMLAssociationEnd(M_associationEnd.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationEnd), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'associationEnd', Result.ClassName, 'TUMLAssociationEnd']));
end;

procedure TUMLLinkEnd._SetassociationEnd(const value: TUMLAssociationEnd);
begin
  M_associationEnd.BoldObject := value;
end;

function TUMLLinkEnd._Get_M_link: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLLinkEnd', 'link', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLLinkEnd._Getlink: TUMLLink;
begin
  Result := TUMLLink(M_link.BoldObject);
  assert(not assigned(Result) or (Result is TUMLLink), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'link', Result.ClassName, 'TUMLLink']));
end;

procedure TUMLLinkEnd._Setlink(const value: TUMLLink);
begin
  M_link.BoldObject := value;
end;

procedure TUMLLinkEndList.Add(NewObject: TUMLLinkEnd);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLLinkEndList.IndexOf(anObject: TUMLLinkEnd): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLLinkEndList.Includes(anObject: TUMLLinkEnd) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLLinkEndList.AddNew: TUMLLinkEnd;
begin
  result := TUMLLinkEnd(InternalAddNew);
end;

procedure TUMLLinkEndList.Insert(index: Integer; NewObject: TUMLLinkEnd);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLLinkEndList.GetBoldObject(index: Integer): TUMLLinkEnd;
begin
  result := TUMLLinkEnd(GetElement(index));
end;

procedure TUMLLinkEndList.SetBoldObject(index: Integer; NewObject: TUMLLinkEnd);
begin;
  SetElement(index, NewObject);
end;

{ TUMLMessage }

function TUMLMessage._Get_M_interaction: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMessage', 'interaction', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLMessage._Getinteraction: TUMLInteraction;
begin
  Result := TUMLInteraction(M_interaction.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInteraction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'interaction', Result.ClassName, 'TUMLInteraction']));
end;

procedure TUMLMessage._Setinteraction(const value: TUMLInteraction);
begin
  M_interaction.BoldObject := value;
end;

function TUMLMessage._Get_M_communicationConnection: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMessage', 'communicationConnection', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLMessage._GetcommunicationConnection: TUMLAssociationRole;
begin
  Result := TUMLAssociationRole(M_communicationConnection.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'communicationConnection', Result.ClassName, 'TUMLAssociationRole']));
end;

procedure TUMLMessage._SetcommunicationConnection(const value: TUMLAssociationRole);
begin
  M_communicationConnection.BoldObject := value;
end;

function TUMLMessage._Get_M_action: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMessage', 'action', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLMessage._Getaction: TUMLAction;
begin
  Result := TUMLAction(M_action.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'action', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLMessage._Setaction(const value: TUMLAction);
begin
  M_action.BoldObject := value;
end;

function TUMLMessage._Getpredecessor: TUMLMessageList;
begin
  assert(ValidateMember('TUMLMessage', 'predecessor', 43, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[43]);
end;

function TUMLMessage._Getpredecessorpredecessormessage3: Tpredecessormessage3List;
begin
  assert(ValidateMember('TUMLMessage', 'predecessorpredecessormessage3', 44, Tpredecessormessage3List));
  Result := Tpredecessormessage3List(BoldMembers[44]);
end;

function TUMLMessage._Getmessage3: TUMLMessageList;
begin
  assert(ValidateMember('TUMLMessage', 'message3', 45, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[45]);
end;

function TUMLMessage._Getmessage3predecessormessage3: Tpredecessormessage3List;
begin
  assert(ValidateMember('TUMLMessage', 'message3predecessormessage3', 46, Tpredecessormessage3List));
  Result := Tpredecessormessage3List(BoldMembers[46]);
end;

function TUMLMessage._Getmessage4: TUMLMessageList;
begin
  assert(ValidateMember('TUMLMessage', 'message4', 47, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[47]);
end;

function TUMLMessage._Get_M_activator: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMessage', 'activator', 48, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[48]);
end;

function TUMLMessage._Getactivator: TUMLMessage;
begin
  Result := TUMLMessage(M_activator.BoldObject);
  assert(not assigned(Result) or (Result is TUMLMessage), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'activator', Result.ClassName, 'TUMLMessage']));
end;

procedure TUMLMessage._Setactivator(const value: TUMLMessage);
begin
  M_activator.BoldObject := value;
end;

function TUMLMessage._Get_M_receiver: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMessage', 'receiver', 49, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[49]);
end;

function TUMLMessage._Getreceiver: TUMLClassifierRole;
begin
  Result := TUMLClassifierRole(M_receiver.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifierRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'receiver', Result.ClassName, 'TUMLClassifierRole']));
end;

procedure TUMLMessage._Setreceiver(const value: TUMLClassifierRole);
begin
  M_receiver.BoldObject := value;
end;

function TUMLMessage._Get_M_sender: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMessage', 'sender', 50, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[50]);
end;

function TUMLMessage._Getsender: TUMLClassifierRole;
begin
  Result := TUMLClassifierRole(M_sender.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifierRole), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'sender', Result.ClassName, 'TUMLClassifierRole']));
end;

procedure TUMLMessage._Setsender(const value: TUMLClassifierRole);
begin
  M_sender.BoldObject := value;
end;

procedure TUMLMessageList.Add(NewObject: TUMLMessage);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLMessageList.IndexOf(anObject: TUMLMessage): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLMessageList.Includes(anObject: TUMLMessage) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLMessageList.AddNew: TUMLMessage;
begin
  result := TUMLMessage(InternalAddNew);
end;

procedure TUMLMessageList.Insert(index: Integer; NewObject: TUMLMessage);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLMessageList.GetBoldObject(index: Integer): TUMLMessage;
begin
  result := TUMLMessage(GetElement(index));
end;

procedure TUMLMessageList.SetBoldObject(index: Integer; NewObject: TUMLMessage);
begin;
  SetElement(index, NewObject);
end;

{ TUMLNamespace }

function TUMLNamespace._GetownedElement: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLNamespace', 'ownedElement', 40, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[40]);
end;

function TUMLNamespace._Getassociations: TUMLAssociationList;
begin
  assert(ValidateMember('TUMLNamespace', 'associations', 41, TUMLAssociationList));
  Result := TUMLAssociationList(BoldMembers[41]);
end;

function TUMLNamespace._Getclasses: TUMLClassList;
begin
  assert(ValidateMember('TUMLNamespace', 'classes', 42, TUMLClassList));
  Result := TUMLClassList(BoldMembers[42]);
end;

function TUMLNamespace._GetallOwnedElement: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLNamespace', 'allOwnedElement', 43, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[43]);
end;

procedure TUMLNamespaceList.Add(NewObject: TUMLNamespace);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLNamespaceList.IndexOf(anObject: TUMLNamespace): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLNamespaceList.Includes(anObject: TUMLNamespace) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLNamespaceList.AddNew: TUMLNamespace;
begin
  result := TUMLNamespace(InternalAddNew);
end;

procedure TUMLNamespaceList.Insert(index: Integer; NewObject: TUMLNamespace);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLNamespaceList.GetBoldObject(index: Integer): TUMLNamespace;
begin
  result := TUMLNamespace(GetElement(index));
end;

procedure TUMLNamespaceList.SetBoldObject(index: Integer; NewObject: TUMLNamespace);
begin;
  SetElement(index, NewObject);
end;

{ TUMLParameter }

function TUMLParameter._Get_M_defaultValue: TBAString;
begin
  assert(ValidateMember('TUMLParameter', 'defaultValue', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLParameter._GetdefaultValue: String;
begin
  Result := M_defaultValue.AsString;
end;

procedure TUMLParameter._SetdefaultValue(const NewValue: String);
begin
  M_defaultValue.AsString := NewValue;
end;

function TUMLParameter._Get_M_kind: TBAParameterDirectionKind;
begin
  assert(ValidateMember('TUMLParameter', 'kind', 41, TBAParameterDirectionKind));
  Result := TBAParameterDirectionKind(BoldMembers[41]);
end;

function TUMLParameter._Getkind: TBoldParameterDirectionKind;
begin
  Result := M_kind.AsParameterDirectionKind;
end;

procedure TUMLParameter._Setkind(const NewValue: TBoldParameterDirectionKind);
begin
  M_kind.AsParameterDirectionKind := NewValue;
end;

function TUMLParameter._Get_M_typeName: TBAString;
begin
  assert(ValidateMember('TUMLParameter', 'typeName', 42, TBAString));
  Result := TBAString(BoldMembers[42]);
end;

function TUMLParameter._GettypeName: String;
begin
  Result := M_typeName.AsString;
end;

procedure TUMLParameter._SettypeName(const NewValue: String);
begin
  M_typeName.AsString := NewValue;
end;

function TUMLParameter._Get_M_event: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLParameter', 'event', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLParameter._Getevent: TUMLEvent;
begin
  Result := TUMLEvent(M_event.BoldObject);
  assert(not assigned(Result) or (Result is TUMLEvent), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'event', Result.ClassName, 'TUMLEvent']));
end;

procedure TUMLParameter._Setevent(const value: TUMLEvent);
begin
  M_event.BoldObject := value;
end;

function TUMLParameter._Getstate: TUMLObjectFlowStateList;
begin
  assert(ValidateMember('TUMLParameter', 'state', 44, TUMLObjectFlowStateList));
  Result := TUMLObjectFlowStateList(BoldMembers[44]);
end;

function TUMLParameter._Getparameterstate: TparameterstateList;
begin
  assert(ValidateMember('TUMLParameter', 'parameterstate', 45, TparameterstateList));
  Result := TparameterstateList(BoldMembers[45]);
end;

function TUMLParameter._Get_M_type_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLParameter', 'type_', 46, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[46]);
end;

function TUMLParameter._Gettype_: TUMLClassifier;
begin
  Result := TUMLClassifier(M_type_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'type_', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLParameter._Settype_(const value: TUMLClassifier);
begin
  M_type_.BoldObject := value;
end;

function TUMLParameter._Get_M_behavioralFeature: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLParameter', 'behavioralFeature', 47, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[47]);
end;

function TUMLParameter._GetbehavioralFeature: TUMLBehavioralFeature;
begin
  Result := TUMLBehavioralFeature(M_behavioralFeature.BoldObject);
  assert(not assigned(Result) or (Result is TUMLBehavioralFeature), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'behavioralFeature', Result.ClassName, 'TUMLBehavioralFeature']));
end;

procedure TUMLParameter._SetbehavioralFeature(const value: TUMLBehavioralFeature);
begin
  M_behavioralFeature.BoldObject := value;
end;

procedure TUMLParameterList.Add(NewObject: TUMLParameter);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLParameterList.IndexOf(anObject: TUMLParameter): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLParameterList.Includes(anObject: TUMLParameter) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLParameterList.AddNew: TUMLParameter;
begin
  result := TUMLParameter(InternalAddNew);
end;

procedure TUMLParameterList.Insert(index: Integer; NewObject: TUMLParameter);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLParameterList.GetBoldObject(index: Integer): TUMLParameter;
begin
  result := TUMLParameter(GetElement(index));
end;

procedure TUMLParameterList.SetBoldObject(index: Integer; NewObject: TUMLParameter);
begin;
  SetElement(index, NewObject);
end;

function TUMLParameter.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    42: result := _typeName_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLParameter.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    42: result := _typeName_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLPartition }

function TUMLPartition._Getcontents: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLPartition', 'contents', 40, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[40]);
end;

function TUMLPartition._Getcontentscontentspartition: TcontentspartitionList;
begin
  assert(ValidateMember('TUMLPartition', 'contentscontentspartition', 41, TcontentspartitionList));
  Result := TcontentspartitionList(BoldMembers[41]);
end;

function TUMLPartition._Get_M_activityGraph: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLPartition', 'activityGraph', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLPartition._GetactivityGraph: TUMLActivityGraph;
begin
  Result := TUMLActivityGraph(M_activityGraph.BoldObject);
  assert(not assigned(Result) or (Result is TUMLActivityGraph), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'activityGraph', Result.ClassName, 'TUMLActivityGraph']));
end;

procedure TUMLPartition._SetactivityGraph(const value: TUMLActivityGraph);
begin
  M_activityGraph.BoldObject := value;
end;

procedure TUMLPartitionList.Add(NewObject: TUMLPartition);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLPartitionList.IndexOf(anObject: TUMLPartition): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLPartitionList.Includes(anObject: TUMLPartition) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLPartitionList.AddNew: TUMLPartition;
begin
  result := TUMLPartition(InternalAddNew);
end;

procedure TUMLPartitionList.Insert(index: Integer; NewObject: TUMLPartition);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLPartitionList.GetBoldObject(index: Integer): TUMLPartition;
begin
  result := TUMLPartition(GetElement(index));
end;

procedure TUMLPartitionList.SetBoldObject(index: Integer; NewObject: TUMLPartition);
begin;
  SetElement(index, NewObject);
end;

{ TUMLRelationship }

procedure TUMLRelationshipList.Add(NewObject: TUMLRelationship);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLRelationshipList.IndexOf(anObject: TUMLRelationship): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLRelationshipList.Includes(anObject: TUMLRelationship) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLRelationshipList.AddNew: TUMLRelationship;
begin
  result := TUMLRelationship(InternalAddNew);
end;

procedure TUMLRelationshipList.Insert(index: Integer; NewObject: TUMLRelationship);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLRelationshipList.GetBoldObject(index: Integer): TUMLRelationship;
begin
  result := TUMLRelationship(GetElement(index));
end;

procedure TUMLRelationshipList.SetBoldObject(index: Integer; NewObject: TUMLRelationship);
begin;
  SetElement(index, NewObject);
end;

{ TUMLStateMachine }

function TUMLStateMachine._Gettransitions: TUMLTransitionList;
begin
  assert(ValidateMember('TUMLStateMachine', 'transitions', 40, TUMLTransitionList));
  Result := TUMLTransitionList(BoldMembers[40]);
end;

function TUMLStateMachine._GetsubMachineState: TUMLSubmachineStateList;
begin
  assert(ValidateMember('TUMLStateMachine', 'subMachineState', 41, TUMLSubmachineStateList));
  Result := TUMLSubmachineStateList(BoldMembers[41]);
end;

function TUMLStateMachine._Get_M_context: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStateMachine', 'context', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLStateMachine._Getcontext: TUMLModelElement;
begin
  Result := TUMLModelElement(M_context.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'context', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLStateMachine._Setcontext(const value: TUMLModelElement);
begin
  M_context.BoldObject := value;
end;

function TUMLStateMachine._Get_M_top: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStateMachine', 'top', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLStateMachine._Gettop: TUMLState;
begin
  Result := TUMLState(M_top.BoldObject);
  assert(not assigned(Result) or (Result is TUMLState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'top', Result.ClassName, 'TUMLState']));
end;

procedure TUMLStateMachine._Settop(const value: TUMLState);
begin
  M_top.BoldObject := value;
end;

procedure TUMLStateMachineList.Add(NewObject: TUMLStateMachine);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStateMachineList.IndexOf(anObject: TUMLStateMachine): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStateMachineList.Includes(anObject: TUMLStateMachine) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStateMachineList.AddNew: TUMLStateMachine;
begin
  result := TUMLStateMachine(InternalAddNew);
end;

procedure TUMLStateMachineList.Insert(index: Integer; NewObject: TUMLStateMachine);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStateMachineList.GetBoldObject(index: Integer): TUMLStateMachine;
begin
  result := TUMLStateMachine(GetElement(index));
end;

procedure TUMLStateMachineList.SetBoldObject(index: Integer; NewObject: TUMLStateMachine);
begin;
  SetElement(index, NewObject);
end;

{ TUMLStateVertex }

function TUMLStateVertex._Get_M_container: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStateVertex', 'container', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLStateVertex._Getcontainer: TUMLCompositeState;
begin
  Result := TUMLCompositeState(M_container.BoldObject);
  assert(not assigned(Result) or (Result is TUMLCompositeState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'container', Result.ClassName, 'TUMLCompositeState']));
end;

procedure TUMLStateVertex._Setcontainer(const value: TUMLCompositeState);
begin
  M_container.BoldObject := value;
end;

function TUMLStateVertex._Getoutgoing: TUMLTransitionList;
begin
  assert(ValidateMember('TUMLStateVertex', 'outgoing', 41, TUMLTransitionList));
  Result := TUMLTransitionList(BoldMembers[41]);
end;

function TUMLStateVertex._Getincoming: TUMLTransitionList;
begin
  assert(ValidateMember('TUMLStateVertex', 'incoming', 42, TUMLTransitionList));
  Result := TUMLTransitionList(BoldMembers[42]);
end;

procedure TUMLStateVertexList.Add(NewObject: TUMLStateVertex);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStateVertexList.IndexOf(anObject: TUMLStateVertex): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStateVertexList.Includes(anObject: TUMLStateVertex) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStateVertexList.AddNew: TUMLStateVertex;
begin
  result := TUMLStateVertex(InternalAddNew);
end;

procedure TUMLStateVertexList.Insert(index: Integer; NewObject: TUMLStateVertex);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStateVertexList.GetBoldObject(index: Integer): TUMLStateVertex;
begin
  result := TUMLStateVertex(GetElement(index));
end;

procedure TUMLStateVertexList.SetBoldObject(index: Integer; NewObject: TUMLStateVertex);
begin;
  SetElement(index, NewObject);
end;

{ TUMLStimulus }

function TUMLStimulus._Getargument: TUMLInstanceList;
begin
  assert(ValidateMember('TUMLStimulus', 'argument', 40, TUMLInstanceList));
  Result := TUMLInstanceList(BoldMembers[40]);
end;

function TUMLStimulus._Getargumentstimulus1: Targumentstimulus1List;
begin
  assert(ValidateMember('TUMLStimulus', 'argumentstimulus1', 41, Targumentstimulus1List));
  Result := Targumentstimulus1List(BoldMembers[41]);
end;

function TUMLStimulus._Get_M_dispatchAction: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStimulus', 'dispatchAction', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLStimulus._GetdispatchAction: TUMLAction;
begin
  Result := TUMLAction(M_dispatchAction.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'dispatchAction', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLStimulus._SetdispatchAction(const value: TUMLAction);
begin
  M_dispatchAction.BoldObject := value;
end;

function TUMLStimulus._Get_M_communicationLink: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStimulus', 'communicationLink', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLStimulus._GetcommunicationLink: TUMLLink;
begin
  Result := TUMLLink(M_communicationLink.BoldObject);
  assert(not assigned(Result) or (Result is TUMLLink), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'communicationLink', Result.ClassName, 'TUMLLink']));
end;

procedure TUMLStimulus._SetcommunicationLink(const value: TUMLLink);
begin
  M_communicationLink.BoldObject := value;
end;

function TUMLStimulus._Get_M_receiver: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStimulus', 'receiver', 44, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[44]);
end;

function TUMLStimulus._Getreceiver: TUMLInstance;
begin
  Result := TUMLInstance(M_receiver.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'receiver', Result.ClassName, 'TUMLInstance']));
end;

procedure TUMLStimulus._Setreceiver(const value: TUMLInstance);
begin
  M_receiver.BoldObject := value;
end;

function TUMLStimulus._Get_M_sender: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStimulus', 'sender', 45, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[45]);
end;

function TUMLStimulus._Getsender: TUMLInstance;
begin
  Result := TUMLInstance(M_sender.BoldObject);
  assert(not assigned(Result) or (Result is TUMLInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'sender', Result.ClassName, 'TUMLInstance']));
end;

procedure TUMLStimulus._Setsender(const value: TUMLInstance);
begin
  M_sender.BoldObject := value;
end;

procedure TUMLStimulusList.Add(NewObject: TUMLStimulus);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStimulusList.IndexOf(anObject: TUMLStimulus): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStimulusList.Includes(anObject: TUMLStimulus) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStimulusList.AddNew: TUMLStimulus;
begin
  result := TUMLStimulus(InternalAddNew);
end;

procedure TUMLStimulusList.Insert(index: Integer; NewObject: TUMLStimulus);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStimulusList.GetBoldObject(index: Integer): TUMLStimulus;
begin
  result := TUMLStimulus(GetElement(index));
end;

procedure TUMLStimulusList.SetBoldObject(index: Integer; NewObject: TUMLStimulus);
begin;
  SetElement(index, NewObject);
end;

{ TUMLTaggedValue }

function TUMLTaggedValue._Get_M_tag: TBAString;
begin
  assert(ValidateMember('TUMLTaggedValue', 'tag', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLTaggedValue._Gettag: String;
begin
  Result := M_tag.AsString;
end;

procedure TUMLTaggedValue._Settag(const NewValue: String);
begin
  M_tag.AsString := NewValue;
end;

function TUMLTaggedValue._Get_M_value: TBAString;
begin
  assert(ValidateMember('TUMLTaggedValue', 'value', 41, TBAString));
  Result := TBAString(BoldMembers[41]);
end;

function TUMLTaggedValue._Getvalue: String;
begin
  Result := M_value.AsString;
end;

procedure TUMLTaggedValue._Setvalue(const NewValue: String);
begin
  M_value.AsString := NewValue;
end;

function TUMLTaggedValue._Get_M_modelElement: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTaggedValue', 'modelElement', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLTaggedValue._GetmodelElement: TUMLModelElement;
begin
  Result := TUMLModelElement(M_modelElement.BoldObject);
  assert(not assigned(Result) or (Result is TUMLModelElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'modelElement', Result.ClassName, 'TUMLModelElement']));
end;

procedure TUMLTaggedValue._SetmodelElement(const value: TUMLModelElement);
begin
  M_modelElement.BoldObject := value;
end;

function TUMLTaggedValue._Get_M_stereotype_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTaggedValue', 'stereotype_', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLTaggedValue._Getstereotype_: TUMLStereotype;
begin
  Result := TUMLStereotype(M_stereotype_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStereotype), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'stereotype_', Result.ClassName, 'TUMLStereotype']));
end;

procedure TUMLTaggedValue._Setstereotype_(const value: TUMLStereotype);
begin
  M_stereotype_.BoldObject := value;
end;

procedure TUMLTaggedValueList.Add(NewObject: TUMLTaggedValue);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLTaggedValueList.IndexOf(anObject: TUMLTaggedValue): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLTaggedValueList.Includes(anObject: TUMLTaggedValue) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLTaggedValueList.AddNew: TUMLTaggedValue;
begin
  result := TUMLTaggedValue(InternalAddNew);
end;

procedure TUMLTaggedValueList.Insert(index: Integer; NewObject: TUMLTaggedValue);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLTaggedValueList.GetBoldObject(index: Integer): TUMLTaggedValue;
begin
  result := TUMLTaggedValue(GetElement(index));
end;

procedure TUMLTaggedValueList.SetBoldObject(index: Integer; NewObject: TUMLTaggedValue);
begin;
  SetElement(index, NewObject);
end;

{ TUMLTransition }

function TUMLTransition._Get_M_State: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'State', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLTransition._GetState: TUMLState;
begin
  Result := TUMLState(M_State.BoldObject);
  assert(not assigned(Result) or (Result is TUMLState), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'State', Result.ClassName, 'TUMLState']));
end;

procedure TUMLTransition._SetState(const value: TUMLState);
begin
  M_State.BoldObject := value;
end;

function TUMLTransition._Get_M_trigger: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'trigger', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLTransition._Gettrigger: TUMLEvent;
begin
  Result := TUMLEvent(M_trigger.BoldObject);
  assert(not assigned(Result) or (Result is TUMLEvent), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'trigger', Result.ClassName, 'TUMLEvent']));
end;

procedure TUMLTransition._Settrigger(const value: TUMLEvent);
begin
  M_trigger.BoldObject := value;
end;

function TUMLTransition._Get_M_effect: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'effect', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLTransition._Geteffect: TUMLAction;
begin
  Result := TUMLAction(M_effect.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'effect', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLTransition._Seteffect(const value: TUMLAction);
begin
  M_effect.BoldObject := value;
end;

function TUMLTransition._Get_M_stateMachine: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'stateMachine', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLTransition._GetstateMachine: TUMLStateMachine;
begin
  Result := TUMLStateMachine(M_stateMachine.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStateMachine), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'stateMachine', Result.ClassName, 'TUMLStateMachine']));
end;

procedure TUMLTransition._SetstateMachine(const value: TUMLStateMachine);
begin
  M_stateMachine.BoldObject := value;
end;

function TUMLTransition._Get_M_source: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'source', 44, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[44]);
end;

function TUMLTransition._Getsource: TUMLStateVertex;
begin
  Result := TUMLStateVertex(M_source.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStateVertex), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'source', Result.ClassName, 'TUMLStateVertex']));
end;

procedure TUMLTransition._Setsource(const value: TUMLStateVertex);
begin
  M_source.BoldObject := value;
end;

function TUMLTransition._Get_M_target: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'target', 45, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[45]);
end;

function TUMLTransition._Gettarget: TUMLStateVertex;
begin
  Result := TUMLStateVertex(M_target.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStateVertex), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'target', Result.ClassName, 'TUMLStateVertex']));
end;

procedure TUMLTransition._Settarget(const value: TUMLStateVertex);
begin
  M_target.BoldObject := value;
end;

function TUMLTransition._Get_M_guard: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLTransition', 'guard', 46, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[46]);
end;

function TUMLTransition._Getguard: TUMLGuard;
begin
  Result := TUMLGuard(M_guard.BoldObject);
  assert(not assigned(Result) or (Result is TUMLGuard), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'guard', Result.ClassName, 'TUMLGuard']));
end;

procedure TUMLTransition._Setguard(const value: TUMLGuard);
begin
  M_guard.BoldObject := value;
end;

procedure TUMLTransitionList.Add(NewObject: TUMLTransition);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLTransitionList.IndexOf(anObject: TUMLTransition): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLTransitionList.Includes(anObject: TUMLTransition) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLTransitionList.AddNew: TUMLTransition;
begin
  result := TUMLTransition(InternalAddNew);
end;

procedure TUMLTransitionList.Insert(index: Integer; NewObject: TUMLTransition);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLTransitionList.GetBoldObject(index: Integer): TUMLTransition;
begin
  result := TUMLTransition(GetElement(index));
end;

procedure TUMLTransitionList.SetBoldObject(index: Integer; NewObject: TUMLTransition);
begin;
  SetElement(index, NewObject);
end;

{ TUMLActionSequence }

function TUMLActionSequence._Getaction: TUMLActionList;
begin
  assert(ValidateMember('TUMLActionSequence', 'action', 52, TUMLActionList));
  Result := TUMLActionList(BoldMembers[52]);
end;

procedure TUMLActionSequenceList.Add(NewObject: TUMLActionSequence);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLActionSequenceList.IndexOf(anObject: TUMLActionSequence): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLActionSequenceList.Includes(anObject: TUMLActionSequence) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLActionSequenceList.AddNew: TUMLActionSequence;
begin
  result := TUMLActionSequence(InternalAddNew);
end;

procedure TUMLActionSequenceList.Insert(index: Integer; NewObject: TUMLActionSequence);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLActionSequenceList.GetBoldObject(index: Integer): TUMLActionSequence;
begin
  result := TUMLActionSequence(GetElement(index));
end;

procedure TUMLActionSequenceList.SetBoldObject(index: Integer; NewObject: TUMLActionSequence);
begin;
  SetElement(index, NewObject);
end;

{ TUMLCallAction }

function TUMLCallAction._Get_M_operation: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLCallAction', 'operation', 52, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[52]);
end;

function TUMLCallAction._Getoperation: TUMLOperation;
begin
  Result := TUMLOperation(M_operation.BoldObject);
  assert(not assigned(Result) or (Result is TUMLOperation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'operation', Result.ClassName, 'TUMLOperation']));
end;

procedure TUMLCallAction._Setoperation(const value: TUMLOperation);
begin
  M_operation.BoldObject := value;
end;

procedure TUMLCallActionList.Add(NewObject: TUMLCallAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCallActionList.IndexOf(anObject: TUMLCallAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCallActionList.Includes(anObject: TUMLCallAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCallActionList.AddNew: TUMLCallAction;
begin
  result := TUMLCallAction(InternalAddNew);
end;

procedure TUMLCallActionList.Insert(index: Integer; NewObject: TUMLCallAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCallActionList.GetBoldObject(index: Integer): TUMLCallAction;
begin
  result := TUMLCallAction(GetElement(index));
end;

procedure TUMLCallActionList.SetBoldObject(index: Integer; NewObject: TUMLCallAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLCreateAction }

function TUMLCreateAction._Get_M_instantiation: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLCreateAction', 'instantiation', 52, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[52]);
end;

function TUMLCreateAction._Getinstantiation: TUMLClassifier;
begin
  Result := TUMLClassifier(M_instantiation.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'instantiation', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLCreateAction._Setinstantiation(const value: TUMLClassifier);
begin
  M_instantiation.BoldObject := value;
end;

procedure TUMLCreateActionList.Add(NewObject: TUMLCreateAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCreateActionList.IndexOf(anObject: TUMLCreateAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCreateActionList.Includes(anObject: TUMLCreateAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCreateActionList.AddNew: TUMLCreateAction;
begin
  result := TUMLCreateAction(InternalAddNew);
end;

procedure TUMLCreateActionList.Insert(index: Integer; NewObject: TUMLCreateAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCreateActionList.GetBoldObject(index: Integer): TUMLCreateAction;
begin
  result := TUMLCreateAction(GetElement(index));
end;

procedure TUMLCreateActionList.SetBoldObject(index: Integer; NewObject: TUMLCreateAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLDestroyAction }

procedure TUMLDestroyActionList.Add(NewObject: TUMLDestroyAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLDestroyActionList.IndexOf(anObject: TUMLDestroyAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLDestroyActionList.Includes(anObject: TUMLDestroyAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLDestroyActionList.AddNew: TUMLDestroyAction;
begin
  result := TUMLDestroyAction(InternalAddNew);
end;

procedure TUMLDestroyActionList.Insert(index: Integer; NewObject: TUMLDestroyAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLDestroyActionList.GetBoldObject(index: Integer): TUMLDestroyAction;
begin
  result := TUMLDestroyAction(GetElement(index));
end;

procedure TUMLDestroyActionList.SetBoldObject(index: Integer; NewObject: TUMLDestroyAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLReturnAction }

procedure TUMLReturnActionList.Add(NewObject: TUMLReturnAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLReturnActionList.IndexOf(anObject: TUMLReturnAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLReturnActionList.Includes(anObject: TUMLReturnAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLReturnActionList.AddNew: TUMLReturnAction;
begin
  result := TUMLReturnAction(InternalAddNew);
end;

procedure TUMLReturnActionList.Insert(index: Integer; NewObject: TUMLReturnAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLReturnActionList.GetBoldObject(index: Integer): TUMLReturnAction;
begin
  result := TUMLReturnAction(GetElement(index));
end;

procedure TUMLReturnActionList.SetBoldObject(index: Integer; NewObject: TUMLReturnAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSendAction }

function TUMLSendAction._Get_M_signal: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLSendAction', 'signal', 52, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[52]);
end;

function TUMLSendAction._Getsignal: TUMLSignal;
begin
  Result := TUMLSignal(M_signal.BoldObject);
  assert(not assigned(Result) or (Result is TUMLSignal), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'signal', Result.ClassName, 'TUMLSignal']));
end;

procedure TUMLSendAction._Setsignal(const value: TUMLSignal);
begin
  M_signal.BoldObject := value;
end;

procedure TUMLSendActionList.Add(NewObject: TUMLSendAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSendActionList.IndexOf(anObject: TUMLSendAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSendActionList.Includes(anObject: TUMLSendAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSendActionList.AddNew: TUMLSendAction;
begin
  result := TUMLSendAction(InternalAddNew);
end;

procedure TUMLSendActionList.Insert(index: Integer; NewObject: TUMLSendAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSendActionList.GetBoldObject(index: Integer): TUMLSendAction;
begin
  result := TUMLSendAction(GetElement(index));
end;

procedure TUMLSendActionList.SetBoldObject(index: Integer; NewObject: TUMLSendAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLTerminateAction }

procedure TUMLTerminateActionList.Add(NewObject: TUMLTerminateAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLTerminateActionList.IndexOf(anObject: TUMLTerminateAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLTerminateActionList.Includes(anObject: TUMLTerminateAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLTerminateActionList.AddNew: TUMLTerminateAction;
begin
  result := TUMLTerminateAction(InternalAddNew);
end;

procedure TUMLTerminateActionList.Insert(index: Integer; NewObject: TUMLTerminateAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLTerminateActionList.GetBoldObject(index: Integer): TUMLTerminateAction;
begin
  result := TUMLTerminateAction(GetElement(index));
end;

procedure TUMLTerminateActionList.SetBoldObject(index: Integer; NewObject: TUMLTerminateAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLUninterpretedAction }

procedure TUMLUninterpretedActionList.Add(NewObject: TUMLUninterpretedAction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLUninterpretedActionList.IndexOf(anObject: TUMLUninterpretedAction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLUninterpretedActionList.Includes(anObject: TUMLUninterpretedAction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLUninterpretedActionList.AddNew: TUMLUninterpretedAction;
begin
  result := TUMLUninterpretedAction(InternalAddNew);
end;

procedure TUMLUninterpretedActionList.Insert(index: Integer; NewObject: TUMLUninterpretedAction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLUninterpretedActionList.GetBoldObject(index: Integer): TUMLUninterpretedAction;
begin
  result := TUMLUninterpretedAction(GetElement(index));
end;

procedure TUMLUninterpretedActionList.SetBoldObject(index: Integer; NewObject: TUMLUninterpretedAction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLAssociationEndRole }

function TUMLAssociationEndRole._Get_M_collaborationMultiplicity: TBAString;
begin
  assert(ValidateMember('TUMLAssociationEndRole', 'collaborationMultiplicity', 57, TBAString));
  Result := TBAString(BoldMembers[57]);
end;

function TUMLAssociationEndRole._GetcollaborationMultiplicity: String;
begin
  Result := M_collaborationMultiplicity.AsString;
end;

procedure TUMLAssociationEndRole._SetcollaborationMultiplicity(const NewValue: String);
begin
  M_collaborationMultiplicity.AsString := NewValue;
end;

function TUMLAssociationEndRole._Get_M_base: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAssociationEndRole', 'base', 58, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[58]);
end;

function TUMLAssociationEndRole._Getbase: TUMLAssociationEnd;
begin
  Result := TUMLAssociationEnd(M_base.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationEnd), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'base', Result.ClassName, 'TUMLAssociationEnd']));
end;

procedure TUMLAssociationEndRole._Setbase(const value: TUMLAssociationEnd);
begin
  M_base.BoldObject := value;
end;

function TUMLAssociationEndRole._GetavailableQualifier: TUMLAttributeList;
begin
  assert(ValidateMember('TUMLAssociationEndRole', 'availableQualifier', 59, TUMLAttributeList));
  Result := TUMLAttributeList(BoldMembers[59]);
end;

function TUMLAssociationEndRole._GetassociationEndRoleavailableQualifier: TassociationEndRoleavailableQualifierList;
begin
  assert(ValidateMember('TUMLAssociationEndRole', 'associationEndRoleavailableQualifier', 60, TassociationEndRoleavailableQualifierList));
  Result := TassociationEndRoleavailableQualifierList(BoldMembers[60]);
end;

procedure TUMLAssociationEndRoleList.Add(NewObject: TUMLAssociationEndRole);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAssociationEndRoleList.IndexOf(anObject: TUMLAssociationEndRole): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAssociationEndRoleList.Includes(anObject: TUMLAssociationEndRole) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAssociationEndRoleList.AddNew: TUMLAssociationEndRole;
begin
  result := TUMLAssociationEndRole(InternalAddNew);
end;

procedure TUMLAssociationEndRoleList.Insert(index: Integer; NewObject: TUMLAssociationEndRole);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAssociationEndRoleList.GetBoldObject(index: Integer): TUMLAssociationEndRole;
begin
  result := TUMLAssociationEndRole(GetElement(index));
end;

procedure TUMLAssociationEndRoleList.SetBoldObject(index: Integer; NewObject: TUMLAssociationEndRole);
begin;
  SetElement(index, NewObject);
end;

{ TUMLCallEvent }

function TUMLCallEvent._Get_M_operation: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLCallEvent', 'operation', 44, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[44]);
end;

function TUMLCallEvent._Getoperation: TUMLOperation;
begin
  Result := TUMLOperation(M_operation.BoldObject);
  assert(not assigned(Result) or (Result is TUMLOperation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'operation', Result.ClassName, 'TUMLOperation']));
end;

procedure TUMLCallEvent._Setoperation(const value: TUMLOperation);
begin
  M_operation.BoldObject := value;
end;

procedure TUMLCallEventList.Add(NewObject: TUMLCallEvent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCallEventList.IndexOf(anObject: TUMLCallEvent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCallEventList.Includes(anObject: TUMLCallEvent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCallEventList.AddNew: TUMLCallEvent;
begin
  result := TUMLCallEvent(InternalAddNew);
end;

procedure TUMLCallEventList.Insert(index: Integer; NewObject: TUMLCallEvent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCallEventList.GetBoldObject(index: Integer): TUMLCallEvent;
begin
  result := TUMLCallEvent(GetElement(index));
end;

procedure TUMLCallEventList.SetBoldObject(index: Integer; NewObject: TUMLCallEvent);
begin;
  SetElement(index, NewObject);
end;

{ TUMLChangeEvent }

function TUMLChangeEvent._Get_M_changeExpression: TBAString;
begin
  assert(ValidateMember('TUMLChangeEvent', 'changeExpression', 44, TBAString));
  Result := TBAString(BoldMembers[44]);
end;

function TUMLChangeEvent._GetchangeExpression: String;
begin
  Result := M_changeExpression.AsString;
end;

procedure TUMLChangeEvent._SetchangeExpression(const NewValue: String);
begin
  M_changeExpression.AsString := NewValue;
end;

procedure TUMLChangeEventList.Add(NewObject: TUMLChangeEvent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLChangeEventList.IndexOf(anObject: TUMLChangeEvent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLChangeEventList.Includes(anObject: TUMLChangeEvent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLChangeEventList.AddNew: TUMLChangeEvent;
begin
  result := TUMLChangeEvent(InternalAddNew);
end;

procedure TUMLChangeEventList.Insert(index: Integer; NewObject: TUMLChangeEvent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLChangeEventList.GetBoldObject(index: Integer): TUMLChangeEvent;
begin
  result := TUMLChangeEvent(GetElement(index));
end;

procedure TUMLChangeEventList.SetBoldObject(index: Integer; NewObject: TUMLChangeEvent);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSignalEvent }

function TUMLSignalEvent._Get_M_signal: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLSignalEvent', 'signal', 44, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[44]);
end;

function TUMLSignalEvent._Getsignal: TUMLSignal;
begin
  Result := TUMLSignal(M_signal.BoldObject);
  assert(not assigned(Result) or (Result is TUMLSignal), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'signal', Result.ClassName, 'TUMLSignal']));
end;

procedure TUMLSignalEvent._Setsignal(const value: TUMLSignal);
begin
  M_signal.BoldObject := value;
end;

procedure TUMLSignalEventList.Add(NewObject: TUMLSignalEvent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSignalEventList.IndexOf(anObject: TUMLSignalEvent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSignalEventList.Includes(anObject: TUMLSignalEvent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSignalEventList.AddNew: TUMLSignalEvent;
begin
  result := TUMLSignalEvent(InternalAddNew);
end;

procedure TUMLSignalEventList.Insert(index: Integer; NewObject: TUMLSignalEvent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSignalEventList.GetBoldObject(index: Integer): TUMLSignalEvent;
begin
  result := TUMLSignalEvent(GetElement(index));
end;

procedure TUMLSignalEventList.SetBoldObject(index: Integer; NewObject: TUMLSignalEvent);
begin;
  SetElement(index, NewObject);
end;

{ TUMLTimeEvent }

function TUMLTimeEvent._Get_M_when: TBAString;
begin
  assert(ValidateMember('TUMLTimeEvent', 'when', 44, TBAString));
  Result := TBAString(BoldMembers[44]);
end;

function TUMLTimeEvent._Getwhen: String;
begin
  Result := M_when.AsString;
end;

procedure TUMLTimeEvent._Setwhen(const NewValue: String);
begin
  M_when.AsString := NewValue;
end;

procedure TUMLTimeEventList.Add(NewObject: TUMLTimeEvent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLTimeEventList.IndexOf(anObject: TUMLTimeEvent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLTimeEventList.Includes(anObject: TUMLTimeEvent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLTimeEventList.AddNew: TUMLTimeEvent;
begin
  result := TUMLTimeEvent(InternalAddNew);
end;

procedure TUMLTimeEventList.Insert(index: Integer; NewObject: TUMLTimeEvent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLTimeEventList.GetBoldObject(index: Integer): TUMLTimeEvent;
begin
  result := TUMLTimeEvent(GetElement(index));
end;

procedure TUMLTimeEventList.SetBoldObject(index: Integer; NewObject: TUMLTimeEvent);
begin;
  SetElement(index, NewObject);
end;

{ TUMLBehavioralFeature }

function TUMLBehavioralFeature._Get_M_isQuery: TBABoolean;
begin
  assert(ValidateMember('TUMLBehavioralFeature', 'isQuery', 44, TBABoolean));
  Result := TBABoolean(BoldMembers[44]);
end;

function TUMLBehavioralFeature._GetisQuery: boolean;
begin
  Result := M_isQuery.AsBoolean;
end;

procedure TUMLBehavioralFeature._SetisQuery(const NewValue: boolean);
begin
  M_isQuery.AsBoolean := NewValue;
end;

function TUMLBehavioralFeature._GetraisedSignal: TUMLSignalList;
begin
  assert(ValidateMember('TUMLBehavioralFeature', 'raisedSignal', 45, TUMLSignalList));
  Result := TUMLSignalList(BoldMembers[45]);
end;

function TUMLBehavioralFeature._GetcontextraisedSignal: TcontextraisedSignalList;
begin
  assert(ValidateMember('TUMLBehavioralFeature', 'contextraisedSignal', 46, TcontextraisedSignalList));
  Result := TcontextraisedSignalList(BoldMembers[46]);
end;

function TUMLBehavioralFeature._Getparameter: TUMLParameterList;
begin
  assert(ValidateMember('TUMLBehavioralFeature', 'parameter', 47, TUMLParameterList));
  Result := TUMLParameterList(BoldMembers[47]);
end;

procedure TUMLBehavioralFeatureList.Add(NewObject: TUMLBehavioralFeature);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLBehavioralFeatureList.IndexOf(anObject: TUMLBehavioralFeature): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLBehavioralFeatureList.Includes(anObject: TUMLBehavioralFeature) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLBehavioralFeatureList.AddNew: TUMLBehavioralFeature;
begin
  result := TUMLBehavioralFeature(InternalAddNew);
end;

procedure TUMLBehavioralFeatureList.Insert(index: Integer; NewObject: TUMLBehavioralFeature);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLBehavioralFeatureList.GetBoldObject(index: Integer): TUMLBehavioralFeature;
begin
  result := TUMLBehavioralFeature(GetElement(index));
end;

procedure TUMLBehavioralFeatureList.SetBoldObject(index: Integer; NewObject: TUMLBehavioralFeature);
begin;
  SetElement(index, NewObject);
end;

{ TUMLStructuralFeature }

function TUMLStructuralFeature._Get_M_multiplicity: TBAString;
begin
  assert(ValidateMember('TUMLStructuralFeature', 'multiplicity', 44, TBAString));
  Result := TBAString(BoldMembers[44]);
end;

function TUMLStructuralFeature._Getmultiplicity: String;
begin
  Result := M_multiplicity.AsString;
end;

procedure TUMLStructuralFeature._Setmultiplicity(const NewValue: String);
begin
  M_multiplicity.AsString := NewValue;
end;

function TUMLStructuralFeature._Get_M_changeability: TBAChangeableKind;
begin
  assert(ValidateMember('TUMLStructuralFeature', 'changeability', 45, TBAChangeableKind));
  Result := TBAChangeableKind(BoldMembers[45]);
end;

function TUMLStructuralFeature._Getchangeability: TChangeableKind;
begin
  Result := M_changeability.AsChangeableKind;
end;

procedure TUMLStructuralFeature._Setchangeability(const NewValue: TChangeableKind);
begin
  M_changeability.AsChangeableKind := NewValue;
end;

function TUMLStructuralFeature._Get_M_targetScope: TBAScopeKind;
begin
  assert(ValidateMember('TUMLStructuralFeature', 'targetScope', 46, TBAScopeKind));
  Result := TBAScopeKind(BoldMembers[46]);
end;

function TUMLStructuralFeature._GettargetScope: TScopeKind;
begin
  Result := M_targetScope.AsScopeKind;
end;

procedure TUMLStructuralFeature._SettargetScope(const NewValue: TScopeKind);
begin
  M_targetScope.AsScopeKind := NewValue;
end;

function TUMLStructuralFeature._Get_M_typeName: TBAString;
begin
  assert(ValidateMember('TUMLStructuralFeature', 'typeName', 47, TBAString));
  Result := TBAString(BoldMembers[47]);
end;

function TUMLStructuralFeature._GettypeName: String;
begin
  Result := M_typeName.AsString;
end;

procedure TUMLStructuralFeature._SettypeName(const NewValue: String);
begin
  M_typeName.AsString := NewValue;
end;

function TUMLStructuralFeature._Get_M_type_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLStructuralFeature', 'type_', 48, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[48]);
end;

function TUMLStructuralFeature._Gettype_: TUMLClassifier;
begin
  Result := TUMLClassifier(M_type_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'type_', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLStructuralFeature._Settype_(const value: TUMLClassifier);
begin
  M_type_.BoldObject := value;
end;

procedure TUMLStructuralFeatureList.Add(NewObject: TUMLStructuralFeature);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStructuralFeatureList.IndexOf(anObject: TUMLStructuralFeature): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStructuralFeatureList.Includes(anObject: TUMLStructuralFeature) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStructuralFeatureList.AddNew: TUMLStructuralFeature;
begin
  result := TUMLStructuralFeature(InternalAddNew);
end;

procedure TUMLStructuralFeatureList.Insert(index: Integer; NewObject: TUMLStructuralFeature);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStructuralFeatureList.GetBoldObject(index: Integer): TUMLStructuralFeature;
begin
  result := TUMLStructuralFeature(GetElement(index));
end;

procedure TUMLStructuralFeatureList.SetBoldObject(index: Integer; NewObject: TUMLStructuralFeature);
begin;
  SetElement(index, NewObject);
end;

function TUMLStructuralFeature.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    47: result := _typeName_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLStructuralFeature.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    47: result := _typeName_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLComponentInstance }

function TUMLComponentInstance._Getresident_: TUMLInstanceList;
begin
  assert(ValidateMember('TUMLComponentInstance', 'resident_', 50, TUMLInstanceList));
  Result := TUMLInstanceList(BoldMembers[50]);
end;

function TUMLComponentInstance._Get_M_nodeInstance: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLComponentInstance', 'nodeInstance', 51, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[51]);
end;

function TUMLComponentInstance._GetnodeInstance: TUMLNodeInstance;
begin
  Result := TUMLNodeInstance(M_nodeInstance.BoldObject);
  assert(not assigned(Result) or (Result is TUMLNodeInstance), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'nodeInstance', Result.ClassName, 'TUMLNodeInstance']));
end;

procedure TUMLComponentInstance._SetnodeInstance(const value: TUMLNodeInstance);
begin
  M_nodeInstance.BoldObject := value;
end;

procedure TUMLComponentInstanceList.Add(NewObject: TUMLComponentInstance);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLComponentInstanceList.IndexOf(anObject: TUMLComponentInstance): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLComponentInstanceList.Includes(anObject: TUMLComponentInstance) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLComponentInstanceList.AddNew: TUMLComponentInstance;
begin
  result := TUMLComponentInstance(InternalAddNew);
end;

procedure TUMLComponentInstanceList.Insert(index: Integer; NewObject: TUMLComponentInstance);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLComponentInstanceList.GetBoldObject(index: Integer): TUMLComponentInstance;
begin
  result := TUMLComponentInstance(GetElement(index));
end;

procedure TUMLComponentInstanceList.SetBoldObject(index: Integer; NewObject: TUMLComponentInstance);
begin;
  SetElement(index, NewObject);
end;

{ TUMLDataValue }

procedure TUMLDataValueList.Add(NewObject: TUMLDataValue);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLDataValueList.IndexOf(anObject: TUMLDataValue): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLDataValueList.Includes(anObject: TUMLDataValue) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLDataValueList.AddNew: TUMLDataValue;
begin
  result := TUMLDataValue(InternalAddNew);
end;

procedure TUMLDataValueList.Insert(index: Integer; NewObject: TUMLDataValue);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLDataValueList.GetBoldObject(index: Integer): TUMLDataValue;
begin
  result := TUMLDataValue(GetElement(index));
end;

procedure TUMLDataValueList.SetBoldObject(index: Integer; NewObject: TUMLDataValue);
begin;
  SetElement(index, NewObject);
end;

{ TUMLNodeInstance }

function TUMLNodeInstance._Getresident_: TUMLComponentInstanceList;
begin
  assert(ValidateMember('TUMLNodeInstance', 'resident_', 50, TUMLComponentInstanceList));
  Result := TUMLComponentInstanceList(BoldMembers[50]);
end;

procedure TUMLNodeInstanceList.Add(NewObject: TUMLNodeInstance);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLNodeInstanceList.IndexOf(anObject: TUMLNodeInstance): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLNodeInstanceList.Includes(anObject: TUMLNodeInstance) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLNodeInstanceList.AddNew: TUMLNodeInstance;
begin
  result := TUMLNodeInstance(InternalAddNew);
end;

procedure TUMLNodeInstanceList.Insert(index: Integer; NewObject: TUMLNodeInstance);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLNodeInstanceList.GetBoldObject(index: Integer): TUMLNodeInstance;
begin
  result := TUMLNodeInstance(GetElement(index));
end;

procedure TUMLNodeInstanceList.SetBoldObject(index: Integer; NewObject: TUMLNodeInstance);
begin;
  SetElement(index, NewObject);
end;

{ TUMLObject }

function TUMLObject._Get_M_link: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLObject', 'link', 50, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[50]);
end;

function TUMLObject._Getlink: TUMLLink;
begin
  Result := TUMLLink(M_link.BoldObject);
  assert(not assigned(Result) or (Result is TUMLLink), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'link', Result.ClassName, 'TUMLLink']));
end;

procedure TUMLObject._Setlink(const value: TUMLLink);
begin
  M_link.BoldObject := value;
end;

procedure TUMLObjectList.Add(NewObject: TUMLObject);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLObjectList.IndexOf(anObject: TUMLObject): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLObjectList.Includes(anObject: TUMLObject) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLObjectList.AddNew: TUMLObject;
begin
  result := TUMLObject(InternalAddNew);
end;

procedure TUMLObjectList.Insert(index: Integer; NewObject: TUMLObject);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLObjectList.GetBoldObject(index: Integer): TUMLObject;
begin
  result := TUMLObject(GetElement(index));
end;

procedure TUMLObjectList.SetBoldObject(index: Integer; NewObject: TUMLObject);
begin;
  SetElement(index, NewObject);
end;

{ TUMLUseCaseInstance }

procedure TUMLUseCaseInstanceList.Add(NewObject: TUMLUseCaseInstance);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLUseCaseInstanceList.IndexOf(anObject: TUMLUseCaseInstance): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLUseCaseInstanceList.Includes(anObject: TUMLUseCaseInstance) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLUseCaseInstanceList.AddNew: TUMLUseCaseInstance;
begin
  result := TUMLUseCaseInstance(InternalAddNew);
end;

procedure TUMLUseCaseInstanceList.Insert(index: Integer; NewObject: TUMLUseCaseInstance);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLUseCaseInstanceList.GetBoldObject(index: Integer): TUMLUseCaseInstance;
begin
  result := TUMLUseCaseInstance(GetElement(index));
end;

procedure TUMLUseCaseInstanceList.SetBoldObject(index: Integer; NewObject: TUMLUseCaseInstance);
begin;
  SetElement(index, NewObject);
end;

{ TUMLGeneralizableElement }

function TUMLGeneralizableElement._Get_M_isRoot: TBABoolean;
begin
  assert(ValidateMember('TUMLGeneralizableElement', 'isRoot', 44, TBABoolean));
  Result := TBABoolean(BoldMembers[44]);
end;

function TUMLGeneralizableElement._GetisRoot: boolean;
begin
  Result := M_isRoot.AsBoolean;
end;

procedure TUMLGeneralizableElement._SetisRoot(const NewValue: boolean);
begin
  M_isRoot.AsBoolean := NewValue;
end;

function TUMLGeneralizableElement._Get_M_isLeaf: TBABoolean;
begin
  assert(ValidateMember('TUMLGeneralizableElement', 'isLeaf', 45, TBABoolean));
  Result := TBABoolean(BoldMembers[45]);
end;

function TUMLGeneralizableElement._GetisLeaf: boolean;
begin
  Result := M_isLeaf.AsBoolean;
end;

procedure TUMLGeneralizableElement._SetisLeaf(const NewValue: boolean);
begin
  M_isLeaf.AsBoolean := NewValue;
end;

function TUMLGeneralizableElement._Get_M_isAbstract: TBABoolean;
begin
  assert(ValidateMember('TUMLGeneralizableElement', 'isAbstract', 46, TBABoolean));
  Result := TBABoolean(BoldMembers[46]);
end;

function TUMLGeneralizableElement._GetisAbstract: boolean;
begin
  Result := M_isAbstract.AsBoolean;
end;

procedure TUMLGeneralizableElement._SetisAbstract(const NewValue: boolean);
begin
  M_isAbstract.AsBoolean := NewValue;
end;

function TUMLGeneralizableElement._Getspecialization: TUMLGeneralizationList;
begin
  assert(ValidateMember('TUMLGeneralizableElement', 'specialization', 47, TUMLGeneralizationList));
  Result := TUMLGeneralizationList(BoldMembers[47]);
end;

function TUMLGeneralizableElement._Getgeneralization: TUMLGeneralizationList;
begin
  assert(ValidateMember('TUMLGeneralizableElement', 'generalization', 48, TUMLGeneralizationList));
  Result := TUMLGeneralizationList(BoldMembers[48]);
end;

procedure TUMLGeneralizableElementList.Add(NewObject: TUMLGeneralizableElement);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLGeneralizableElementList.IndexOf(anObject: TUMLGeneralizableElement): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLGeneralizableElementList.Includes(anObject: TUMLGeneralizableElement) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLGeneralizableElementList.AddNew: TUMLGeneralizableElement;
begin
  result := TUMLGeneralizableElement(InternalAddNew);
end;

procedure TUMLGeneralizableElementList.Insert(index: Integer; NewObject: TUMLGeneralizableElement);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLGeneralizableElementList.GetBoldObject(index: Integer): TUMLGeneralizableElement;
begin
  result := TUMLGeneralizableElement(GetElement(index));
end;

procedure TUMLGeneralizableElementList.SetBoldObject(index: Integer; NewObject: TUMLGeneralizableElement);
begin;
  SetElement(index, NewObject);
end;

{ TUMLDependency }

function TUMLDependency._Getsupplier: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLDependency', 'supplier', 40, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[40]);
end;

function TUMLDependency._GetsuppliersuppliersupplierDependency: TsuppliersupplierDependencyList;
begin
  assert(ValidateMember('TUMLDependency', 'suppliersuppliersupplierDependency', 41, TsuppliersupplierDependencyList));
  Result := TsuppliersupplierDependencyList(BoldMembers[41]);
end;

function TUMLDependency._Getclient: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLDependency', 'client', 42, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[42]);
end;

function TUMLDependency._GetclientclientclientDependency: TclientclientDependencyList;
begin
  assert(ValidateMember('TUMLDependency', 'clientclientclientDependency', 43, TclientclientDependencyList));
  Result := TclientclientDependencyList(BoldMembers[43]);
end;

procedure TUMLDependencyList.Add(NewObject: TUMLDependency);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLDependencyList.IndexOf(anObject: TUMLDependency): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLDependencyList.Includes(anObject: TUMLDependency) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLDependencyList.AddNew: TUMLDependency;
begin
  result := TUMLDependency(InternalAddNew);
end;

procedure TUMLDependencyList.Insert(index: Integer; NewObject: TUMLDependency);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLDependencyList.GetBoldObject(index: Integer): TUMLDependency;
begin
  result := TUMLDependency(GetElement(index));
end;

procedure TUMLDependencyList.SetBoldObject(index: Integer; NewObject: TUMLDependency);
begin;
  SetElement(index, NewObject);
end;

{ TUMLExtend }

function TUMLExtend._Get_M_condition: TBAString;
begin
  assert(ValidateMember('TUMLExtend', 'condition', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLExtend._Getcondition: String;
begin
  Result := M_condition.AsString;
end;

procedure TUMLExtend._Setcondition(const NewValue: String);
begin
  M_condition.AsString := NewValue;
end;

function TUMLExtend._GetextensionPoint: TUMLExtensionPointList;
begin
  assert(ValidateMember('TUMLExtend', 'extensionPoint', 41, TUMLExtensionPointList));
  Result := TUMLExtensionPointList(BoldMembers[41]);
end;

function TUMLExtend._GetextensionPointextend: TextensionPointextendList;
begin
  assert(ValidateMember('TUMLExtend', 'extensionPointextend', 42, TextensionPointextendList));
  Result := TextensionPointextendList(BoldMembers[42]);
end;

function TUMLExtend._Get_M_base: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLExtend', 'base', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLExtend._Getbase: TUMLUseCase;
begin
  Result := TUMLUseCase(M_base.BoldObject);
  assert(not assigned(Result) or (Result is TUMLUseCase), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'base', Result.ClassName, 'TUMLUseCase']));
end;

procedure TUMLExtend._Setbase(const value: TUMLUseCase);
begin
  M_base.BoldObject := value;
end;

function TUMLExtend._Get_M_extension: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLExtend', 'extension', 44, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[44]);
end;

function TUMLExtend._Getextension: TUMLUseCase;
begin
  Result := TUMLUseCase(M_extension.BoldObject);
  assert(not assigned(Result) or (Result is TUMLUseCase), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'extension', Result.ClassName, 'TUMLUseCase']));
end;

procedure TUMLExtend._Setextension(const value: TUMLUseCase);
begin
  M_extension.BoldObject := value;
end;

procedure TUMLExtendList.Add(NewObject: TUMLExtend);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLExtendList.IndexOf(anObject: TUMLExtend): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLExtendList.Includes(anObject: TUMLExtend) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLExtendList.AddNew: TUMLExtend;
begin
  result := TUMLExtend(InternalAddNew);
end;

procedure TUMLExtendList.Insert(index: Integer; NewObject: TUMLExtend);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLExtendList.GetBoldObject(index: Integer): TUMLExtend;
begin
  result := TUMLExtend(GetElement(index));
end;

procedure TUMLExtendList.SetBoldObject(index: Integer; NewObject: TUMLExtend);
begin;
  SetElement(index, NewObject);
end;

{ TUMLFlow }

function TUMLFlow._Getsource: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLFlow', 'source', 40, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[40]);
end;

function TUMLFlow._GetsourcesourceFlowsource: TsourceFlowsourceList;
begin
  assert(ValidateMember('TUMLFlow', 'sourcesourceFlowsource', 41, TsourceFlowsourceList));
  Result := TsourceFlowsourceList(BoldMembers[41]);
end;

function TUMLFlow._Gettarget: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLFlow', 'target', 42, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[42]);
end;

function TUMLFlow._GettargettargetFlowtarget: TtargetFlowtargetList;
begin
  assert(ValidateMember('TUMLFlow', 'targettargetFlowtarget', 43, TtargetFlowtargetList));
  Result := TtargetFlowtargetList(BoldMembers[43]);
end;

procedure TUMLFlowList.Add(NewObject: TUMLFlow);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLFlowList.IndexOf(anObject: TUMLFlow): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLFlowList.Includes(anObject: TUMLFlow) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLFlowList.AddNew: TUMLFlow;
begin
  result := TUMLFlow(InternalAddNew);
end;

procedure TUMLFlowList.Insert(index: Integer; NewObject: TUMLFlow);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLFlowList.GetBoldObject(index: Integer): TUMLFlow;
begin
  result := TUMLFlow(GetElement(index));
end;

procedure TUMLFlowList.SetBoldObject(index: Integer; NewObject: TUMLFlow);
begin;
  SetElement(index, NewObject);
end;

{ TUMLGeneralization }

function TUMLGeneralization._Get_M_discriminator: TBAString;
begin
  assert(ValidateMember('TUMLGeneralization', 'discriminator', 40, TBAString));
  Result := TBAString(BoldMembers[40]);
end;

function TUMLGeneralization._Getdiscriminator: String;
begin
  Result := M_discriminator.AsString;
end;

procedure TUMLGeneralization._Setdiscriminator(const NewValue: String);
begin
  M_discriminator.AsString := NewValue;
end;

function TUMLGeneralization._Get_M_parent: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLGeneralization', 'parent', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLGeneralization._Getparent: TUMLGeneralizableElement;
begin
  Result := TUMLGeneralizableElement(M_parent.BoldObject);
  assert(not assigned(Result) or (Result is TUMLGeneralizableElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'parent', Result.ClassName, 'TUMLGeneralizableElement']));
end;

procedure TUMLGeneralization._Setparent(const value: TUMLGeneralizableElement);
begin
  M_parent.BoldObject := value;
end;

function TUMLGeneralization._Get_M_child: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLGeneralization', 'child', 42, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[42]);
end;

function TUMLGeneralization._Getchild: TUMLGeneralizableElement;
begin
  Result := TUMLGeneralizableElement(M_child.BoldObject);
  assert(not assigned(Result) or (Result is TUMLGeneralizableElement), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'child', Result.ClassName, 'TUMLGeneralizableElement']));
end;

procedure TUMLGeneralization._Setchild(const value: TUMLGeneralizableElement);
begin
  M_child.BoldObject := value;
end;

function TUMLGeneralization._Get_M_powertype: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLGeneralization', 'powertype', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLGeneralization._Getpowertype: TUMLClassifier;
begin
  Result := TUMLClassifier(M_powertype.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'powertype', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLGeneralization._Setpowertype(const value: TUMLClassifier);
begin
  M_powertype.BoldObject := value;
end;

procedure TUMLGeneralizationList.Add(NewObject: TUMLGeneralization);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLGeneralizationList.IndexOf(anObject: TUMLGeneralization): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLGeneralizationList.Includes(anObject: TUMLGeneralization) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLGeneralizationList.AddNew: TUMLGeneralization;
begin
  result := TUMLGeneralization(InternalAddNew);
end;

procedure TUMLGeneralizationList.Insert(index: Integer; NewObject: TUMLGeneralization);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLGeneralizationList.GetBoldObject(index: Integer): TUMLGeneralization;
begin
  result := TUMLGeneralization(GetElement(index));
end;

procedure TUMLGeneralizationList.SetBoldObject(index: Integer; NewObject: TUMLGeneralization);
begin;
  SetElement(index, NewObject);
end;

{ TUMLInclude }

function TUMLInclude._Get_M_base: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLInclude', 'base', 40, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[40]);
end;

function TUMLInclude._Getbase: TUMLUseCase;
begin
  Result := TUMLUseCase(M_base.BoldObject);
  assert(not assigned(Result) or (Result is TUMLUseCase), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'base', Result.ClassName, 'TUMLUseCase']));
end;

procedure TUMLInclude._Setbase(const value: TUMLUseCase);
begin
  M_base.BoldObject := value;
end;

function TUMLInclude._Get_M_addition: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLInclude', 'addition', 41, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[41]);
end;

function TUMLInclude._Getaddition: TUMLUseCase;
begin
  Result := TUMLUseCase(M_addition.BoldObject);
  assert(not assigned(Result) or (Result is TUMLUseCase), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'addition', Result.ClassName, 'TUMLUseCase']));
end;

procedure TUMLInclude._Setaddition(const value: TUMLUseCase);
begin
  M_addition.BoldObject := value;
end;

procedure TUMLIncludeList.Add(NewObject: TUMLInclude);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLIncludeList.IndexOf(anObject: TUMLInclude): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLIncludeList.Includes(anObject: TUMLInclude) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLIncludeList.AddNew: TUMLInclude;
begin
  result := TUMLInclude(InternalAddNew);
end;

procedure TUMLIncludeList.Insert(index: Integer; NewObject: TUMLInclude);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLIncludeList.GetBoldObject(index: Integer): TUMLInclude;
begin
  result := TUMLInclude(GetElement(index));
end;

procedure TUMLIncludeList.SetBoldObject(index: Integer; NewObject: TUMLInclude);
begin;
  SetElement(index, NewObject);
end;

{ TUMLActivityGraph }

function TUMLActivityGraph._Getpartition_: TUMLPartitionList;
begin
  assert(ValidateMember('TUMLActivityGraph', 'partition_', 44, TUMLPartitionList));
  Result := TUMLPartitionList(BoldMembers[44]);
end;

procedure TUMLActivityGraphList.Add(NewObject: TUMLActivityGraph);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLActivityGraphList.IndexOf(anObject: TUMLActivityGraph): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLActivityGraphList.Includes(anObject: TUMLActivityGraph) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLActivityGraphList.AddNew: TUMLActivityGraph;
begin
  result := TUMLActivityGraph(InternalAddNew);
end;

procedure TUMLActivityGraphList.Insert(index: Integer; NewObject: TUMLActivityGraph);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLActivityGraphList.GetBoldObject(index: Integer): TUMLActivityGraph;
begin
  result := TUMLActivityGraph(GetElement(index));
end;

procedure TUMLActivityGraphList.SetBoldObject(index: Integer; NewObject: TUMLActivityGraph);
begin;
  SetElement(index, NewObject);
end;

{ TUMLPseudostate }

function TUMLPseudostate._Get_M_kind: TBAPseudostateKind;
begin
  assert(ValidateMember('TUMLPseudostate', 'kind', 43, TBAPseudostateKind));
  Result := TBAPseudostateKind(BoldMembers[43]);
end;

function TUMLPseudostate._Getkind: TPseudostateKind;
begin
  Result := M_kind.asPseudostateKind;
end;

procedure TUMLPseudostate._Setkind(const NewValue: TPseudostateKind);
begin
  M_kind.asPseudostateKind := NewValue;
end;

procedure TUMLPseudostateList.Add(NewObject: TUMLPseudostate);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLPseudostateList.IndexOf(anObject: TUMLPseudostate): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLPseudostateList.Includes(anObject: TUMLPseudostate) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLPseudostateList.AddNew: TUMLPseudostate;
begin
  result := TUMLPseudostate(InternalAddNew);
end;

procedure TUMLPseudostateList.Insert(index: Integer; NewObject: TUMLPseudostate);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLPseudostateList.GetBoldObject(index: Integer): TUMLPseudostate;
begin
  result := TUMLPseudostate(GetElement(index));
end;

procedure TUMLPseudostateList.SetBoldObject(index: Integer; NewObject: TUMLPseudostate);
begin;
  SetElement(index, NewObject);
end;

{ TUMLState }

function TUMLState._Get_M_entry: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLState', 'entry', 43, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[43]);
end;

function TUMLState._Getentry: TUMLAction;
begin
  Result := TUMLAction(M_entry.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'entry', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLState._Setentry(const value: TUMLAction);
begin
  M_entry.BoldObject := value;
end;

function TUMLState._GetinternalTransition: TUMLTransitionList;
begin
  assert(ValidateMember('TUMLState', 'internalTransition', 44, TUMLTransitionList));
  Result := TUMLTransitionList(BoldMembers[44]);
end;

function TUMLState._Get_M_doActivity: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLState', 'doActivity', 45, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[45]);
end;

function TUMLState._GetdoActivity: TUMLAction;
begin
  Result := TUMLAction(M_doActivity.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'doActivity', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLState._SetdoActivity(const value: TUMLAction);
begin
  M_doActivity.BoldObject := value;
end;

function TUMLState._Get_M_exit: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLState', 'exit', 46, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[46]);
end;

function TUMLState._Getexit: TUMLAction;
begin
  Result := TUMLAction(M_exit.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAction), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'exit', Result.ClassName, 'TUMLAction']));
end;

procedure TUMLState._Setexit(const value: TUMLAction);
begin
  M_exit.BoldObject := value;
end;

function TUMLState._GetdeferrableEvent: TUMLEventList;
begin
  assert(ValidateMember('TUMLState', 'deferrableEvent', 47, TUMLEventList));
  Result := TUMLEventList(BoldMembers[47]);
end;

function TUMLState._GetstatedeferrableEvent: TstatedeferrableEventList;
begin
  assert(ValidateMember('TUMLState', 'statedeferrableEvent', 48, TstatedeferrableEventList));
  Result := TstatedeferrableEventList(BoldMembers[48]);
end;

function TUMLState._Get_M_stateMachine: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLState', 'stateMachine', 49, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[49]);
end;

function TUMLState._GetstateMachine: TUMLStateMachine;
begin
  Result := TUMLStateMachine(M_stateMachine.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStateMachine), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'stateMachine', Result.ClassName, 'TUMLStateMachine']));
end;

procedure TUMLState._SetstateMachine(const value: TUMLStateMachine);
begin
  M_stateMachine.BoldObject := value;
end;

function TUMLState._GetclassifierInState: TUMLClassifierInStateList;
begin
  assert(ValidateMember('TUMLState', 'classifierInState', 50, TUMLClassifierInStateList));
  Result := TUMLClassifierInStateList(BoldMembers[50]);
end;

function TUMLState._GetclassifierInStateinState: TclassifierInStateinStateList;
begin
  assert(ValidateMember('TUMLState', 'classifierInStateinState', 51, TclassifierInStateinStateList));
  Result := TclassifierInStateinStateList(BoldMembers[51]);
end;

procedure TUMLStateList.Add(NewObject: TUMLState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStateList.IndexOf(anObject: TUMLState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStateList.Includes(anObject: TUMLState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStateList.AddNew: TUMLState;
begin
  result := TUMLState(InternalAddNew);
end;

procedure TUMLStateList.Insert(index: Integer; NewObject: TUMLState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStateList.GetBoldObject(index: Integer): TUMLState;
begin
  result := TUMLState(GetElement(index));
end;

procedure TUMLStateList.SetBoldObject(index: Integer; NewObject: TUMLState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLStubState }

function TUMLStubState._Get_M_referenceState: TBAString;
begin
  assert(ValidateMember('TUMLStubState', 'referenceState', 43, TBAString));
  Result := TBAString(BoldMembers[43]);
end;

function TUMLStubState._GetreferenceState: String;
begin
  Result := M_referenceState.AsString;
end;

procedure TUMLStubState._SetreferenceState(const NewValue: String);
begin
  M_referenceState.AsString := NewValue;
end;

procedure TUMLStubStateList.Add(NewObject: TUMLStubState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStubStateList.IndexOf(anObject: TUMLStubState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStubStateList.Includes(anObject: TUMLStubState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStubStateList.AddNew: TUMLStubState;
begin
  result := TUMLStubState(InternalAddNew);
end;

procedure TUMLStubStateList.Insert(index: Integer; NewObject: TUMLStubState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStubStateList.GetBoldObject(index: Integer): TUMLStubState;
begin
  result := TUMLStubState(GetElement(index));
end;

procedure TUMLStubStateList.SetBoldObject(index: Integer; NewObject: TUMLStubState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSynchState }

function TUMLSynchState._Get_M_bound: TBAString;
begin
  assert(ValidateMember('TUMLSynchState', 'bound', 43, TBAString));
  Result := TBAString(BoldMembers[43]);
end;

function TUMLSynchState._Getbound: String;
begin
  Result := M_bound.AsString;
end;

procedure TUMLSynchState._Setbound(const NewValue: String);
begin
  M_bound.AsString := NewValue;
end;

procedure TUMLSynchStateList.Add(NewObject: TUMLSynchState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSynchStateList.IndexOf(anObject: TUMLSynchState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSynchStateList.Includes(anObject: TUMLSynchState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSynchStateList.AddNew: TUMLSynchState;
begin
  result := TUMLSynchState(InternalAddNew);
end;

procedure TUMLSynchStateList.Insert(index: Integer; NewObject: TUMLSynchState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSynchStateList.GetBoldObject(index: Integer): TUMLSynchState;
begin
  result := TUMLSynchState(GetElement(index));
end;

procedure TUMLSynchStateList.SetBoldObject(index: Integer; NewObject: TUMLSynchState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLMethod }

function TUMLMethod._Get_M_body: TBAString;
begin
  assert(ValidateMember('TUMLMethod', 'body', 48, TBAString));
  Result := TBAString(BoldMembers[48]);
end;

function TUMLMethod._Getbody: String;
begin
  Result := M_body.AsString;
end;

procedure TUMLMethod._Setbody(const NewValue: String);
begin
  M_body.AsString := NewValue;
end;

function TUMLMethod._Get_M_specification: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLMethod', 'specification', 49, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[49]);
end;

function TUMLMethod._Getspecification: TUMLOperation;
begin
  Result := TUMLOperation(M_specification.BoldObject);
  assert(not assigned(Result) or (Result is TUMLOperation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'specification', Result.ClassName, 'TUMLOperation']));
end;

procedure TUMLMethod._Setspecification(const value: TUMLOperation);
begin
  M_specification.BoldObject := value;
end;

procedure TUMLMethodList.Add(NewObject: TUMLMethod);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLMethodList.IndexOf(anObject: TUMLMethod): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLMethodList.Includes(anObject: TUMLMethod) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLMethodList.AddNew: TUMLMethod;
begin
  result := TUMLMethod(InternalAddNew);
end;

procedure TUMLMethodList.Insert(index: Integer; NewObject: TUMLMethod);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLMethodList.GetBoldObject(index: Integer): TUMLMethod;
begin
  result := TUMLMethod(GetElement(index));
end;

procedure TUMLMethodList.SetBoldObject(index: Integer; NewObject: TUMLMethod);
begin;
  SetElement(index, NewObject);
end;

{ TUMLOperation }

function TUMLOperation._Get_M_concurrency: TBACallConcurrencyKind;
begin
  assert(ValidateMember('TUMLOperation', 'concurrency', 48, TBACallConcurrencyKind));
  Result := TBACallConcurrencyKind(BoldMembers[48]);
end;

function TUMLOperation._Getconcurrency: TCallConcurrencyKind;
begin
  Result := M_concurrency.asCallConcurrencyKind;
end;

procedure TUMLOperation._Setconcurrency(const NewValue: TCallConcurrencyKind);
begin
  M_concurrency.asCallConcurrencyKind := NewValue;
end;

function TUMLOperation._Get_M_isRoot: TBABoolean;
begin
  assert(ValidateMember('TUMLOperation', 'isRoot', 49, TBABoolean));
  Result := TBABoolean(BoldMembers[49]);
end;

function TUMLOperation._GetisRoot: boolean;
begin
  Result := M_isRoot.AsBoolean;
end;

procedure TUMLOperation._SetisRoot(const NewValue: boolean);
begin
  M_isRoot.AsBoolean := NewValue;
end;

function TUMLOperation._Get_M_isLeaf: TBABoolean;
begin
  assert(ValidateMember('TUMLOperation', 'isLeaf', 50, TBABoolean));
  Result := TBABoolean(BoldMembers[50]);
end;

function TUMLOperation._GetisLeaf: boolean;
begin
  Result := M_isLeaf.AsBoolean;
end;

procedure TUMLOperation._SetisLeaf(const NewValue: boolean);
begin
  M_isLeaf.AsBoolean := NewValue;
end;

function TUMLOperation._Get_M_isAbstract: TBABoolean;
begin
  assert(ValidateMember('TUMLOperation', 'isAbstract', 51, TBABoolean));
  Result := TBABoolean(BoldMembers[51]);
end;

function TUMLOperation._GetisAbstract: boolean;
begin
  Result := M_isAbstract.AsBoolean;
end;

procedure TUMLOperation._SetisAbstract(const NewValue: boolean);
begin
  M_isAbstract.AsBoolean := NewValue;
end;

function TUMLOperation._Get_M_specification: TBAString;
begin
  assert(ValidateMember('TUMLOperation', 'specification', 52, TBAString));
  Result := TBAString(BoldMembers[52]);
end;

function TUMLOperation._Getspecification: String;
begin
  Result := M_specification.AsString;
end;

procedure TUMLOperation._Setspecification(const NewValue: String);
begin
  M_specification.AsString := NewValue;
end;

function TUMLOperation._Getoccurrences: TUMLCallEventList;
begin
  assert(ValidateMember('TUMLOperation', 'occurrences', 53, TUMLCallEventList));
  Result := TUMLCallEventList(BoldMembers[53]);
end;

function TUMLOperation._GetcallAction: TUMLCallActionList;
begin
  assert(ValidateMember('TUMLOperation', 'callAction', 54, TUMLCallActionList));
  Result := TUMLCallActionList(BoldMembers[54]);
end;

function TUMLOperation._Getcollaboration_: TUMLCollaborationList;
begin
  assert(ValidateMember('TUMLOperation', 'collaboration_', 55, TUMLCollaborationList));
  Result := TUMLCollaborationList(BoldMembers[55]);
end;

function TUMLOperation._Getmethod: TUMLMethodList;
begin
  assert(ValidateMember('TUMLOperation', 'method', 56, TUMLMethodList));
  Result := TUMLMethodList(BoldMembers[56]);
end;

procedure TUMLOperationList.Add(NewObject: TUMLOperation);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLOperationList.IndexOf(anObject: TUMLOperation): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLOperationList.Includes(anObject: TUMLOperation) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLOperationList.AddNew: TUMLOperation;
begin
  result := TUMLOperation(InternalAddNew);
end;

procedure TUMLOperationList.Insert(index: Integer; NewObject: TUMLOperation);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLOperationList.GetBoldObject(index: Integer): TUMLOperation;
begin
  result := TUMLOperation(GetElement(index));
end;

procedure TUMLOperationList.SetBoldObject(index: Integer; NewObject: TUMLOperation);
begin;
  SetElement(index, NewObject);
end;

{ TUMLReception }

function TUMLReception._Get_M_specification: TBAString;
begin
  assert(ValidateMember('TUMLReception', 'specification', 48, TBAString));
  Result := TBAString(BoldMembers[48]);
end;

function TUMLReception._Getspecification: String;
begin
  Result := M_specification.AsString;
end;

procedure TUMLReception._Setspecification(const NewValue: String);
begin
  M_specification.AsString := NewValue;
end;

function TUMLReception._Get_M_isRoot: TBABoolean;
begin
  assert(ValidateMember('TUMLReception', 'isRoot', 49, TBABoolean));
  Result := TBABoolean(BoldMembers[49]);
end;

function TUMLReception._GetisRoot: boolean;
begin
  Result := M_isRoot.AsBoolean;
end;

procedure TUMLReception._SetisRoot(const NewValue: boolean);
begin
  M_isRoot.AsBoolean := NewValue;
end;

function TUMLReception._Get_M_isLeaf: TBABoolean;
begin
  assert(ValidateMember('TUMLReception', 'isLeaf', 50, TBABoolean));
  Result := TBABoolean(BoldMembers[50]);
end;

function TUMLReception._GetisLeaf: boolean;
begin
  Result := M_isLeaf.AsBoolean;
end;

procedure TUMLReception._SetisLeaf(const NewValue: boolean);
begin
  M_isLeaf.AsBoolean := NewValue;
end;

function TUMLReception._Get_M_isAbstract: TBABoolean;
begin
  assert(ValidateMember('TUMLReception', 'isAbstract', 51, TBABoolean));
  Result := TBABoolean(BoldMembers[51]);
end;

function TUMLReception._GetisAbstract: boolean;
begin
  Result := M_isAbstract.AsBoolean;
end;

procedure TUMLReception._SetisAbstract(const NewValue: boolean);
begin
  M_isAbstract.AsBoolean := NewValue;
end;

function TUMLReception._Get_M_signal: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLReception', 'signal', 52, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[52]);
end;

function TUMLReception._Getsignal: TUMLSignal;
begin
  Result := TUMLSignal(M_signal.BoldObject);
  assert(not assigned(Result) or (Result is TUMLSignal), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'signal', Result.ClassName, 'TUMLSignal']));
end;

procedure TUMLReception._Setsignal(const value: TUMLSignal);
begin
  M_signal.BoldObject := value;
end;

procedure TUMLReceptionList.Add(NewObject: TUMLReception);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLReceptionList.IndexOf(anObject: TUMLReception): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLReceptionList.Includes(anObject: TUMLReception) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLReceptionList.AddNew: TUMLReception;
begin
  result := TUMLReception(InternalAddNew);
end;

procedure TUMLReceptionList.Insert(index: Integer; NewObject: TUMLReception);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLReceptionList.GetBoldObject(index: Integer): TUMLReception;
begin
  result := TUMLReception(GetElement(index));
end;

procedure TUMLReceptionList.SetBoldObject(index: Integer; NewObject: TUMLReception);
begin;
  SetElement(index, NewObject);
end;

{ TUMLAttribute }

function TUMLAttribute._Get_M_initialValue: TBAString;
begin
  assert(ValidateMember('TUMLAttribute', 'initialValue', 49, TBAString));
  Result := TBAString(BoldMembers[49]);
end;

function TUMLAttribute._GetinitialValue: String;
begin
  Result := M_initialValue.AsString;
end;

procedure TUMLAttribute._SetinitialValue(const NewValue: String);
begin
  M_initialValue.AsString := NewValue;
end;

function TUMLAttribute._Get_M_persistent: TBABoolean;
begin
  assert(ValidateMember('TUMLAttribute', 'persistent', 50, TBABoolean));
  Result := TBABoolean(BoldMembers[50]);
end;

function TUMLAttribute._Getpersistent: boolean;
begin
  Result := M_persistent.AsBoolean;
end;

procedure TUMLAttribute._Setpersistent(const NewValue: boolean);
begin
  M_persistent.AsBoolean := NewValue;
end;

function TUMLAttribute._GetattributeLink: TUMLAttributeLinkList;
begin
  assert(ValidateMember('TUMLAttribute', 'attributeLink', 51, TUMLAttributeLinkList));
  Result := TUMLAttributeLinkList(BoldMembers[51]);
end;

function TUMLAttribute._GetassociationEndRole: TUMLAssociationEndRoleList;
begin
  assert(ValidateMember('TUMLAttribute', 'associationEndRole', 52, TUMLAssociationEndRoleList));
  Result := TUMLAssociationEndRoleList(BoldMembers[52]);
end;

function TUMLAttribute._GetassociationEndRoleavailableQualifier: TassociationEndRoleavailableQualifierList;
begin
  assert(ValidateMember('TUMLAttribute', 'associationEndRoleavailableQualifier', 53, TassociationEndRoleavailableQualifierList));
  Result := TassociationEndRoleavailableQualifierList(BoldMembers[53]);
end;

function TUMLAttribute._Get_M_associationEnd: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAttribute', 'associationEnd', 54, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[54]);
end;

function TUMLAttribute._GetassociationEnd: TUMLAssociationEnd;
begin
  Result := TUMLAssociationEnd(M_associationEnd.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociationEnd), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'associationEnd', Result.ClassName, 'TUMLAssociationEnd']));
end;

procedure TUMLAttribute._SetassociationEnd(const value: TUMLAssociationEnd);
begin
  M_associationEnd.BoldObject := value;
end;

procedure TUMLAttributeList.Add(NewObject: TUMLAttribute);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAttributeList.IndexOf(anObject: TUMLAttribute): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAttributeList.Includes(anObject: TUMLAttribute) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAttributeList.AddNew: TUMLAttribute;
begin
  result := TUMLAttribute(InternalAddNew);
end;

procedure TUMLAttributeList.Insert(index: Integer; NewObject: TUMLAttribute);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAttributeList.GetBoldObject(index: Integer): TUMLAttribute;
begin
  result := TUMLAttribute(GetElement(index));
end;

procedure TUMLAttributeList.SetBoldObject(index: Integer; NewObject: TUMLAttribute);
begin;
  SetElement(index, NewObject);
end;

function TUMLAttribute.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    50: result := _persistent_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLAttribute.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    50: result := _persistent_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLAssociation }

function TUMLAssociation._Get_M_persistent: TBABoolean;
begin
  assert(ValidateMember('TUMLAssociation', 'persistent', 49, TBABoolean));
  Result := TBABoolean(BoldMembers[49]);
end;

function TUMLAssociation._Getpersistent: boolean;
begin
  Result := M_persistent.AsBoolean;
end;

procedure TUMLAssociation._Setpersistent(const NewValue: boolean);
begin
  M_persistent.AsBoolean := NewValue;
end;

function TUMLAssociation._Get_M_isAssociationClass: TBABoolean;
begin
  assert(ValidateMember('TUMLAssociation', 'isAssociationClass', 50, TBABoolean));
  Result := TBABoolean(BoldMembers[50]);
end;

function TUMLAssociation._GetisAssociationClass: boolean;
begin
  Result := M_isAssociationClass.AsBoolean;
end;

function TUMLAssociation._Getlink: TUMLLinkList;
begin
  assert(ValidateMember('TUMLAssociation', 'link', 51, TUMLLinkList));
  Result := TUMLLinkList(BoldMembers[51]);
end;

function TUMLAssociation._GetassociationRole: TUMLAssociationRoleList;
begin
  assert(ValidateMember('TUMLAssociation', 'associationRole', 52, TUMLAssociationRoleList));
  Result := TUMLAssociationRoleList(BoldMembers[52]);
end;

function TUMLAssociation._Get_M_class_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAssociation', 'class_', 53, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[53]);
end;

function TUMLAssociation._Getclass_: TUMLClass;
begin
  Result := TUMLClass(M_class_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'class_', Result.ClassName, 'TUMLClass']));
end;

procedure TUMLAssociation._Setclass_(const value: TUMLClass);
begin
  M_class_.BoldObject := value;
end;

function TUMLAssociation._Getconnection: TUMLAssociationEndList;
begin
  assert(ValidateMember('TUMLAssociation', 'connection', 54, TUMLAssociationEndList));
  Result := TUMLAssociationEndList(BoldMembers[54]);
end;

procedure TUMLAssociationList.Add(NewObject: TUMLAssociation);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAssociationList.IndexOf(anObject: TUMLAssociation): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAssociationList.Includes(anObject: TUMLAssociation) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAssociationList.AddNew: TUMLAssociation;
begin
  result := TUMLAssociation(InternalAddNew);
end;

procedure TUMLAssociationList.Insert(index: Integer; NewObject: TUMLAssociation);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAssociationList.GetBoldObject(index: Integer): TUMLAssociation;
begin
  result := TUMLAssociation(GetElement(index));
end;

procedure TUMLAssociationList.SetBoldObject(index: Integer; NewObject: TUMLAssociation);
begin;
  SetElement(index, NewObject);
end;

function TUMLAssociation.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    49: result := _persistent_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLAssociation.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    49: result := _persistent_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLClassifier }

function TUMLClassifier._Get_M_persistent: TBABoolean;
begin
  assert(ValidateMember('TUMLClassifier', 'persistent', 49, TBABoolean));
  Result := TBABoolean(BoldMembers[49]);
end;

function TUMLClassifier._Getpersistent: boolean;
begin
  Result := M_persistent.AsBoolean;
end;

procedure TUMLClassifier._Setpersistent(const NewValue: boolean);
begin
  M_persistent.AsBoolean := NewValue;
end;

function TUMLClassifier._GetcreateAction: TUMLCreateActionList;
begin
  assert(ValidateMember('TUMLClassifier', 'createAction', 50, TUMLCreateActionList));
  Result := TUMLCreateActionList(BoldMembers[50]);
end;

function TUMLClassifier._Getinstance: TUMLInstanceList;
begin
  assert(ValidateMember('TUMLClassifier', 'instance', 51, TUMLInstanceList));
  Result := TUMLInstanceList(BoldMembers[51]);
end;

function TUMLClassifier._Getinstanceclassifier: TinstanceclassifierList;
begin
  assert(ValidateMember('TUMLClassifier', 'instanceclassifier', 52, TinstanceclassifierList));
  Result := TinstanceclassifierList(BoldMembers[52]);
end;

function TUMLClassifier._GetclassifierRole_: TUMLClassifierRoleList;
begin
  assert(ValidateMember('TUMLClassifier', 'classifierRole_', 53, TUMLClassifierRoleList));
  Result := TUMLClassifierRoleList(BoldMembers[53]);
end;

function TUMLClassifier._GetclassifierRole_classifierclassifierRole_: TclassifierclassifierRole_List;
begin
  assert(ValidateMember('TUMLClassifier', 'classifierRole_classifierclassifierRole_', 54, TclassifierclassifierRole_List));
  Result := TclassifierclassifierRole_List(BoldMembers[54]);
end;

function TUMLClassifier._Getcollaboration_: TUMLCollaborationList;
begin
  assert(ValidateMember('TUMLClassifier', 'collaboration_', 55, TUMLCollaborationList));
  Result := TUMLCollaborationList(BoldMembers[55]);
end;

function TUMLClassifier._GetclassifierInState: TUMLClassifierInStateList;
begin
  assert(ValidateMember('TUMLClassifier', 'classifierInState', 56, TUMLClassifierInStateList));
  Result := TUMLClassifierInStateList(BoldMembers[56]);
end;

function TUMLClassifier._GetobjectFlowState: TUMLObjectFlowStateList;
begin
  assert(ValidateMember('TUMLClassifier', 'objectFlowState', 57, TUMLObjectFlowStateList));
  Result := TUMLObjectFlowStateList(BoldMembers[57]);
end;

function TUMLClassifier._Getparticipant: TUMLAssociationEndList;
begin
  assert(ValidateMember('TUMLClassifier', 'participant', 58, TUMLAssociationEndList));
  Result := TUMLAssociationEndList(BoldMembers[58]);
end;

function TUMLClassifier._Getparticipantspecification: TparticipantspecificationList;
begin
  assert(ValidateMember('TUMLClassifier', 'participantspecification', 59, TparticipantspecificationList));
  Result := TparticipantspecificationList(BoldMembers[59]);
end;

function TUMLClassifier._Getsubclasses: TUMLClassifierList;
begin
  assert(ValidateMember('TUMLClassifier', 'subclasses', 62, TUMLClassifierList));
  Result := TUMLClassifierList(BoldMembers[62]);
end;

function TUMLClassifier._Get_M_superclass: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLClassifier', 'superclass', 63, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[63]);
end;

function TUMLClassifier._Getsuperclass: TUMLClassifier;
begin
  Result := TUMLClassifier(M_superclass.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'superclass', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLClassifier._Setsuperclass(const value: TUMLClassifier);
begin
  M_superclass.BoldObject := value;
end;

function TUMLClassifier._GetassociationEnd: TUMLAssociationEndList;
begin
  assert(ValidateMember('TUMLClassifier', 'associationEnd', 64, TUMLAssociationEndList));
  Result := TUMLAssociationEndList(BoldMembers[64]);
end;

function TUMLClassifier._GetpowertypeRange: TUMLGeneralizationList;
begin
  assert(ValidateMember('TUMLClassifier', 'powertypeRange', 65, TUMLGeneralizationList));
  Result := TUMLGeneralizationList(BoldMembers[65]);
end;

function TUMLClassifier._GetallFeature: TUMLFeatureList;
begin
  assert(ValidateMember('TUMLClassifier', 'allFeature', 66, TUMLFeatureList));
  Result := TUMLFeatureList(BoldMembers[66]);
end;

function TUMLClassifier._Getfeature: TUMLFeatureList;
begin
  assert(ValidateMember('TUMLClassifier', 'feature', 67, TUMLFeatureList));
  Result := TUMLFeatureList(BoldMembers[67]);
end;

procedure TUMLClassifierList.Add(NewObject: TUMLClassifier);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLClassifierList.IndexOf(anObject: TUMLClassifier): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLClassifierList.Includes(anObject: TUMLClassifier) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLClassifierList.AddNew: TUMLClassifier;
begin
  result := TUMLClassifier(InternalAddNew);
end;

procedure TUMLClassifierList.Insert(index: Integer; NewObject: TUMLClassifier);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLClassifierList.GetBoldObject(index: Integer): TUMLClassifier;
begin
  result := TUMLClassifier(GetElement(index));
end;

procedure TUMLClassifierList.SetBoldObject(index: Integer; NewObject: TUMLClassifier);
begin;
  SetElement(index, NewObject);
end;

function TUMLClassifier.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  case MemberIndex of
    49: result := _persistent_DeriveAndSubscribe;
  else result := inherited GetDeriveMethodForMember(MemberIndex);
  end;
end;

function TUMLClassifier.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  case MemberIndex of
    49: result := _persistent_ReverseDerive;
  else result := inherited GetReverseDeriveMethodForMember(MemberIndex);
  end;
end;

{ TUMLCollaboration }

function TUMLCollaboration._Getinteraction: TUMLInteractionList;
begin
  assert(ValidateMember('TUMLCollaboration', 'interaction', 49, TUMLInteractionList));
  Result := TUMLInteractionList(BoldMembers[49]);
end;

function TUMLCollaboration._GetconstrainingElement: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLCollaboration', 'constrainingElement', 50, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[50]);
end;

function TUMLCollaboration._GetconstrainingElementcollaborationconstrainingElement: TcollaborationconstrainingElementList;
begin
  assert(ValidateMember('TUMLCollaboration', 'constrainingElementcollaborationconstrainingElement', 51, TcollaborationconstrainingElementList));
  Result := TcollaborationconstrainingElementList(BoldMembers[51]);
end;

function TUMLCollaboration._Get_M_representedClassifier: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLCollaboration', 'representedClassifier', 52, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[52]);
end;

function TUMLCollaboration._GetrepresentedClassifier: TUMLClassifier;
begin
  Result := TUMLClassifier(M_representedClassifier.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'representedClassifier', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLCollaboration._SetrepresentedClassifier(const value: TUMLClassifier);
begin
  M_representedClassifier.BoldObject := value;
end;

function TUMLCollaboration._Get_M_representedOperation: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLCollaboration', 'representedOperation', 53, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[53]);
end;

function TUMLCollaboration._GetrepresentedOperation: TUMLOperation;
begin
  Result := TUMLOperation(M_representedOperation.BoldObject);
  assert(not assigned(Result) or (Result is TUMLOperation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'representedOperation', Result.ClassName, 'TUMLOperation']));
end;

procedure TUMLCollaboration._SetrepresentedOperation(const value: TUMLOperation);
begin
  M_representedOperation.BoldObject := value;
end;

procedure TUMLCollaborationList.Add(NewObject: TUMLCollaboration);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCollaborationList.IndexOf(anObject: TUMLCollaboration): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCollaborationList.Includes(anObject: TUMLCollaboration) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCollaborationList.AddNew: TUMLCollaboration;
begin
  result := TUMLCollaboration(InternalAddNew);
end;

procedure TUMLCollaborationList.Insert(index: Integer; NewObject: TUMLCollaboration);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCollaborationList.GetBoldObject(index: Integer): TUMLCollaboration;
begin
  result := TUMLCollaboration(GetElement(index));
end;

procedure TUMLCollaborationList.SetBoldObject(index: Integer; NewObject: TUMLCollaboration);
begin;
  SetElement(index, NewObject);
end;

{ TUMLPackage }

function TUMLPackage._GetelementImport_: TUMLElementImportList;
begin
  assert(ValidateMember('TUMLPackage', 'elementImport_', 49, TUMLElementImportList));
  Result := TUMLElementImportList(BoldMembers[49]);
end;

procedure TUMLPackageList.Add(NewObject: TUMLPackage);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLPackageList.IndexOf(anObject: TUMLPackage): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLPackageList.Includes(anObject: TUMLPackage) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLPackageList.AddNew: TUMLPackage;
begin
  result := TUMLPackage(InternalAddNew);
end;

procedure TUMLPackageList.Insert(index: Integer; NewObject: TUMLPackage);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLPackageList.GetBoldObject(index: Integer): TUMLPackage;
begin
  result := TUMLPackage(GetElement(index));
end;

procedure TUMLPackageList.SetBoldObject(index: Integer; NewObject: TUMLPackage);
begin;
  SetElement(index, NewObject);
end;

{ TUMLStereotype }

function TUMLStereotype._Get_M_icon: TBAString;
begin
  assert(ValidateMember('TUMLStereotype', 'icon', 49, TBAString));
  Result := TBAString(BoldMembers[49]);
end;

function TUMLStereotype._Geticon: String;
begin
  Result := M_icon.AsString;
end;

procedure TUMLStereotype._Seticon(const NewValue: String);
begin
  M_icon.AsString := NewValue;
end;

function TUMLStereotype._Get_M_baseClass: TBAString;
begin
  assert(ValidateMember('TUMLStereotype', 'baseClass', 50, TBAString));
  Result := TBAString(BoldMembers[50]);
end;

function TUMLStereotype._GetbaseClass: String;
begin
  Result := M_baseClass.AsString;
end;

procedure TUMLStereotype._SetbaseClass(const NewValue: String);
begin
  M_baseClass.AsString := NewValue;
end;

function TUMLStereotype._GetstereotypeConstraint: TUMLConstraintList;
begin
  assert(ValidateMember('TUMLStereotype', 'stereotypeConstraint', 51, TUMLConstraintList));
  Result := TUMLConstraintList(BoldMembers[51]);
end;

function TUMLStereotype._GetextendedElement: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLStereotype', 'extendedElement', 52, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[52]);
end;

function TUMLStereotype._GetrequiredTag: TUMLTaggedValueList;
begin
  assert(ValidateMember('TUMLStereotype', 'requiredTag', 53, TUMLTaggedValueList));
  Result := TUMLTaggedValueList(BoldMembers[53]);
end;

procedure TUMLStereotypeList.Add(NewObject: TUMLStereotype);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLStereotypeList.IndexOf(anObject: TUMLStereotype): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLStereotypeList.Includes(anObject: TUMLStereotype) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLStereotypeList.AddNew: TUMLStereotype;
begin
  result := TUMLStereotype(InternalAddNew);
end;

procedure TUMLStereotypeList.Insert(index: Integer; NewObject: TUMLStereotype);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLStereotypeList.GetBoldObject(index: Integer): TUMLStereotype;
begin
  result := TUMLStereotype(GetElement(index));
end;

procedure TUMLStereotypeList.SetBoldObject(index: Integer; NewObject: TUMLStereotype);
begin;
  SetElement(index, NewObject);
end;

{ TUMLAbstraction }

function TUMLAbstraction._Get_M_mapping: TBAString;
begin
  assert(ValidateMember('TUMLAbstraction', 'mapping', 44, TBAString));
  Result := TBAString(BoldMembers[44]);
end;

function TUMLAbstraction._Getmapping: String;
begin
  Result := M_mapping.AsString;
end;

procedure TUMLAbstraction._Setmapping(const NewValue: String);
begin
  M_mapping.AsString := NewValue;
end;

procedure TUMLAbstractionList.Add(NewObject: TUMLAbstraction);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAbstractionList.IndexOf(anObject: TUMLAbstraction): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAbstractionList.Includes(anObject: TUMLAbstraction) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAbstractionList.AddNew: TUMLAbstraction;
begin
  result := TUMLAbstraction(InternalAddNew);
end;

procedure TUMLAbstractionList.Insert(index: Integer; NewObject: TUMLAbstraction);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAbstractionList.GetBoldObject(index: Integer): TUMLAbstraction;
begin
  result := TUMLAbstraction(GetElement(index));
end;

procedure TUMLAbstractionList.SetBoldObject(index: Integer; NewObject: TUMLAbstraction);
begin;
  SetElement(index, NewObject);
end;

{ TUMLBinding }

function TUMLBinding._Getargument: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLBinding', 'argument', 44, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[44]);
end;

procedure TUMLBindingList.Add(NewObject: TUMLBinding);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLBindingList.IndexOf(anObject: TUMLBinding): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLBindingList.Includes(anObject: TUMLBinding) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLBindingList.AddNew: TUMLBinding;
begin
  result := TUMLBinding(InternalAddNew);
end;

procedure TUMLBindingList.Insert(index: Integer; NewObject: TUMLBinding);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLBindingList.GetBoldObject(index: Integer): TUMLBinding;
begin
  result := TUMLBinding(GetElement(index));
end;

procedure TUMLBindingList.SetBoldObject(index: Integer; NewObject: TUMLBinding);
begin;
  SetElement(index, NewObject);
end;

{ TUMLPermission }

procedure TUMLPermissionList.Add(NewObject: TUMLPermission);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLPermissionList.IndexOf(anObject: TUMLPermission): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLPermissionList.Includes(anObject: TUMLPermission) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLPermissionList.AddNew: TUMLPermission;
begin
  result := TUMLPermission(InternalAddNew);
end;

procedure TUMLPermissionList.Insert(index: Integer; NewObject: TUMLPermission);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLPermissionList.GetBoldObject(index: Integer): TUMLPermission;
begin
  result := TUMLPermission(GetElement(index));
end;

procedure TUMLPermissionList.SetBoldObject(index: Integer; NewObject: TUMLPermission);
begin;
  SetElement(index, NewObject);
end;

{ TUMLUsage }

procedure TUMLUsageList.Add(NewObject: TUMLUsage);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLUsageList.IndexOf(anObject: TUMLUsage): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLUsageList.Includes(anObject: TUMLUsage) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLUsageList.AddNew: TUMLUsage;
begin
  result := TUMLUsage(InternalAddNew);
end;

procedure TUMLUsageList.Insert(index: Integer; NewObject: TUMLUsage);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLUsageList.GetBoldObject(index: Integer): TUMLUsage;
begin
  result := TUMLUsage(GetElement(index));
end;

procedure TUMLUsageList.SetBoldObject(index: Integer; NewObject: TUMLUsage);
begin;
  SetElement(index, NewObject);
end;

{ TUMLCompositeState }

function TUMLCompositeState._Get_M_isConcurrent: TBABoolean;
begin
  assert(ValidateMember('TUMLCompositeState', 'isConcurrent', 52, TBABoolean));
  Result := TBABoolean(BoldMembers[52]);
end;

function TUMLCompositeState._GetisConcurrent: boolean;
begin
  Result := M_isConcurrent.AsBoolean;
end;

procedure TUMLCompositeState._SetisConcurrent(const NewValue: boolean);
begin
  M_isConcurrent.AsBoolean := NewValue;
end;

function TUMLCompositeState._Getsubvertex: TUMLStateVertexList;
begin
  assert(ValidateMember('TUMLCompositeState', 'subvertex', 53, TUMLStateVertexList));
  Result := TUMLStateVertexList(BoldMembers[53]);
end;

procedure TUMLCompositeStateList.Add(NewObject: TUMLCompositeState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCompositeStateList.IndexOf(anObject: TUMLCompositeState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCompositeStateList.Includes(anObject: TUMLCompositeState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCompositeStateList.AddNew: TUMLCompositeState;
begin
  result := TUMLCompositeState(InternalAddNew);
end;

procedure TUMLCompositeStateList.Insert(index: Integer; NewObject: TUMLCompositeState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCompositeStateList.GetBoldObject(index: Integer): TUMLCompositeState;
begin
  result := TUMLCompositeState(GetElement(index));
end;

procedure TUMLCompositeStateList.SetBoldObject(index: Integer; NewObject: TUMLCompositeState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLFinalState }

procedure TUMLFinalStateList.Add(NewObject: TUMLFinalState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLFinalStateList.IndexOf(anObject: TUMLFinalState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLFinalStateList.Includes(anObject: TUMLFinalState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLFinalStateList.AddNew: TUMLFinalState;
begin
  result := TUMLFinalState(InternalAddNew);
end;

procedure TUMLFinalStateList.Insert(index: Integer; NewObject: TUMLFinalState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLFinalStateList.GetBoldObject(index: Integer): TUMLFinalState;
begin
  result := TUMLFinalState(GetElement(index));
end;

procedure TUMLFinalStateList.SetBoldObject(index: Integer; NewObject: TUMLFinalState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSimpleState }

procedure TUMLSimpleStateList.Add(NewObject: TUMLSimpleState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSimpleStateList.IndexOf(anObject: TUMLSimpleState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSimpleStateList.Includes(anObject: TUMLSimpleState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSimpleStateList.AddNew: TUMLSimpleState;
begin
  result := TUMLSimpleState(InternalAddNew);
end;

procedure TUMLSimpleStateList.Insert(index: Integer; NewObject: TUMLSimpleState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSimpleStateList.GetBoldObject(index: Integer): TUMLSimpleState;
begin
  result := TUMLSimpleState(GetElement(index));
end;

procedure TUMLSimpleStateList.SetBoldObject(index: Integer; NewObject: TUMLSimpleState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLAssociationRole }

function TUMLAssociationRole._Get_M_multiplicity: TBAString;
begin
  assert(ValidateMember('TUMLAssociationRole', 'multiplicity', 55, TBAString));
  Result := TBAString(BoldMembers[55]);
end;

function TUMLAssociationRole._Getmultiplicity: String;
begin
  Result := M_multiplicity.AsString;
end;

procedure TUMLAssociationRole._Setmultiplicity(const NewValue: String);
begin
  M_multiplicity.AsString := NewValue;
end;

function TUMLAssociationRole._Get_M_base: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLAssociationRole', 'base', 56, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[56]);
end;

function TUMLAssociationRole._Getbase: TUMLAssociation;
begin
  Result := TUMLAssociation(M_base.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'base', Result.ClassName, 'TUMLAssociation']));
end;

procedure TUMLAssociationRole._Setbase(const value: TUMLAssociation);
begin
  M_base.BoldObject := value;
end;

function TUMLAssociationRole._Getmessage_: TUMLMessageList;
begin
  assert(ValidateMember('TUMLAssociationRole', 'message_', 57, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[57]);
end;

procedure TUMLAssociationRoleList.Add(NewObject: TUMLAssociationRole);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLAssociationRoleList.IndexOf(anObject: TUMLAssociationRole): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLAssociationRoleList.Includes(anObject: TUMLAssociationRole) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLAssociationRoleList.AddNew: TUMLAssociationRole;
begin
  result := TUMLAssociationRole(InternalAddNew);
end;

procedure TUMLAssociationRoleList.Insert(index: Integer; NewObject: TUMLAssociationRole);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLAssociationRoleList.GetBoldObject(index: Integer): TUMLAssociationRole;
begin
  result := TUMLAssociationRole(GetElement(index));
end;

procedure TUMLAssociationRoleList.SetBoldObject(index: Integer; NewObject: TUMLAssociationRole);
begin;
  SetElement(index, NewObject);
end;

{ TUMLActor }

procedure TUMLActorList.Add(NewObject: TUMLActor);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLActorList.IndexOf(anObject: TUMLActor): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLActorList.Includes(anObject: TUMLActor) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLActorList.AddNew: TUMLActor;
begin
  result := TUMLActor(InternalAddNew);
end;

procedure TUMLActorList.Insert(index: Integer; NewObject: TUMLActor);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLActorList.GetBoldObject(index: Integer): TUMLActor;
begin
  result := TUMLActor(GetElement(index));
end;

procedure TUMLActorList.SetBoldObject(index: Integer; NewObject: TUMLActor);
begin;
  SetElement(index, NewObject);
end;

{ TUMLClass }

function TUMLClass._Get_M_isActive: TBABoolean;
begin
  assert(ValidateMember('TUMLClass', 'isActive', 68, TBABoolean));
  Result := TBABoolean(BoldMembers[68]);
end;

function TUMLClass._GetisActive: boolean;
begin
  Result := M_isActive.AsBoolean;
end;

procedure TUMLClass._SetisActive(const NewValue: boolean);
begin
  M_isActive.AsBoolean := NewValue;
end;

function TUMLClass._Get_M_isAssociationClass: TBABoolean;
begin
  assert(ValidateMember('TUMLClass', 'isAssociationClass', 69, TBABoolean));
  Result := TBABoolean(BoldMembers[69]);
end;

function TUMLClass._GetisAssociationClass: boolean;
begin
  Result := M_isAssociationClass.AsBoolean;
end;

function TUMLClass._Get_M_association: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLClass', 'association', 70, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[70]);
end;

function TUMLClass._Getassociation: TUMLAssociation;
begin
  Result := TUMLAssociation(M_association.BoldObject);
  assert(not assigned(Result) or (Result is TUMLAssociation), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'association', Result.ClassName, 'TUMLAssociation']));
end;

procedure TUMLClass._Setassociation(const value: TUMLAssociation);
begin
  M_association.BoldObject := value;
end;

procedure TUMLClassList.Add(NewObject: TUMLClass);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLClassList.IndexOf(anObject: TUMLClass): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLClassList.Includes(anObject: TUMLClass) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLClassList.AddNew: TUMLClass;
begin
  result := TUMLClass(InternalAddNew);
end;

procedure TUMLClassList.Insert(index: Integer; NewObject: TUMLClass);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLClassList.GetBoldObject(index: Integer): TUMLClass;
begin
  result := TUMLClass(GetElement(index));
end;

procedure TUMLClassList.SetBoldObject(index: Integer; NewObject: TUMLClass);
begin;
  SetElement(index, NewObject);
end;

{ TUMLClassifierInState }

function TUMLClassifierInState._Get_M_type_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLClassifierInState', 'type_', 68, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[68]);
end;

function TUMLClassifierInState._Gettype_: TUMLClassifier;
begin
  Result := TUMLClassifier(M_type_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'type_', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLClassifierInState._Settype_(const value: TUMLClassifier);
begin
  M_type_.BoldObject := value;
end;

function TUMLClassifierInState._GetinState: TUMLStateList;
begin
  assert(ValidateMember('TUMLClassifierInState', 'inState', 69, TUMLStateList));
  Result := TUMLStateList(BoldMembers[69]);
end;

function TUMLClassifierInState._GetclassifierInStateinState: TclassifierInStateinStateList;
begin
  assert(ValidateMember('TUMLClassifierInState', 'classifierInStateinState', 70, TclassifierInStateinStateList));
  Result := TclassifierInStateinStateList(BoldMembers[70]);
end;

procedure TUMLClassifierInStateList.Add(NewObject: TUMLClassifierInState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLClassifierInStateList.IndexOf(anObject: TUMLClassifierInState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLClassifierInStateList.Includes(anObject: TUMLClassifierInState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLClassifierInStateList.AddNew: TUMLClassifierInState;
begin
  result := TUMLClassifierInState(InternalAddNew);
end;

procedure TUMLClassifierInStateList.Insert(index: Integer; NewObject: TUMLClassifierInState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLClassifierInStateList.GetBoldObject(index: Integer): TUMLClassifierInState;
begin
  result := TUMLClassifierInState(GetElement(index));
end;

procedure TUMLClassifierInStateList.SetBoldObject(index: Integer; NewObject: TUMLClassifierInState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLClassifierRole }

function TUMLClassifierRole._Get_M_multiplicity: TBAString;
begin
  assert(ValidateMember('TUMLClassifierRole', 'multiplicity', 68, TBAString));
  Result := TBAString(BoldMembers[68]);
end;

function TUMLClassifierRole._Getmultiplicity: String;
begin
  Result := M_multiplicity.AsString;
end;

procedure TUMLClassifierRole._Setmultiplicity(const NewValue: String);
begin
  M_multiplicity.AsString := NewValue;
end;

function TUMLClassifierRole._GetavailableFeature: TUMLFeatureList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'availableFeature', 69, TUMLFeatureList));
  Result := TUMLFeatureList(BoldMembers[69]);
end;

function TUMLClassifierRole._GetclassifierRole_availableFeature: TclassifierRole_availableFeatureList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'classifierRole_availableFeature', 70, TclassifierRole_availableFeatureList));
  Result := TclassifierRole_availableFeatureList(BoldMembers[70]);
end;

function TUMLClassifierRole._GetavailableContents: TUMLModelElementList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'availableContents', 71, TUMLModelElementList));
  Result := TUMLModelElementList(BoldMembers[71]);
end;

function TUMLClassifierRole._GetavailableContentsclassifierRoleavailableContents: TclassifierRoleavailableContentsList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'availableContentsclassifierRoleavailableContents', 72, TclassifierRoleavailableContentsList));
  Result := TclassifierRoleavailableContentsList(BoldMembers[72]);
end;

function TUMLClassifierRole._Getclassifier: TUMLClassifierList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'classifier', 73, TUMLClassifierList));
  Result := TUMLClassifierList(BoldMembers[73]);
end;

function TUMLClassifierRole._GetclassifierclassifierclassifierRole_: TclassifierclassifierRole_List;
begin
  assert(ValidateMember('TUMLClassifierRole', 'classifierclassifierclassifierRole_', 74, TclassifierclassifierRole_List));
  Result := TclassifierclassifierRole_List(BoldMembers[74]);
end;

function TUMLClassifierRole._Getmessage1: TUMLMessageList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'message1', 75, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[75]);
end;

function TUMLClassifierRole._Getmessage2: TUMLMessageList;
begin
  assert(ValidateMember('TUMLClassifierRole', 'message2', 76, TUMLMessageList));
  Result := TUMLMessageList(BoldMembers[76]);
end;

procedure TUMLClassifierRoleList.Add(NewObject: TUMLClassifierRole);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLClassifierRoleList.IndexOf(anObject: TUMLClassifierRole): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLClassifierRoleList.Includes(anObject: TUMLClassifierRole) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLClassifierRoleList.AddNew: TUMLClassifierRole;
begin
  result := TUMLClassifierRole(InternalAddNew);
end;

procedure TUMLClassifierRoleList.Insert(index: Integer; NewObject: TUMLClassifierRole);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLClassifierRoleList.GetBoldObject(index: Integer): TUMLClassifierRole;
begin
  result := TUMLClassifierRole(GetElement(index));
end;

procedure TUMLClassifierRoleList.SetBoldObject(index: Integer; NewObject: TUMLClassifierRole);
begin;
  SetElement(index, NewObject);
end;

{ TUMLComponent }

function TUMLComponent._GetimplementationLocation: TUMLElementResidenceList;
begin
  assert(ValidateMember('TUMLComponent', 'implementationLocation', 68, TUMLElementResidenceList));
  Result := TUMLElementResidenceList(BoldMembers[68]);
end;

function TUMLComponent._GetdeploymentLocation: TUMLNodeList;
begin
  assert(ValidateMember('TUMLComponent', 'deploymentLocation', 69, TUMLNodeList));
  Result := TUMLNodeList(BoldMembers[69]);
end;

function TUMLComponent._GetdeploymentLocationresident: TdeploymentLocationresidentList;
begin
  assert(ValidateMember('TUMLComponent', 'deploymentLocationresident', 70, TdeploymentLocationresidentList));
  Result := TdeploymentLocationresidentList(BoldMembers[70]);
end;

procedure TUMLComponentList.Add(NewObject: TUMLComponent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLComponentList.IndexOf(anObject: TUMLComponent): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLComponentList.Includes(anObject: TUMLComponent) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLComponentList.AddNew: TUMLComponent;
begin
  result := TUMLComponent(InternalAddNew);
end;

procedure TUMLComponentList.Insert(index: Integer; NewObject: TUMLComponent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLComponentList.GetBoldObject(index: Integer): TUMLComponent;
begin
  result := TUMLComponent(GetElement(index));
end;

procedure TUMLComponentList.SetBoldObject(index: Integer; NewObject: TUMLComponent);
begin;
  SetElement(index, NewObject);
end;

{ TUMLDataType }

procedure TUMLDataTypeList.Add(NewObject: TUMLDataType);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLDataTypeList.IndexOf(anObject: TUMLDataType): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLDataTypeList.Includes(anObject: TUMLDataType) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLDataTypeList.AddNew: TUMLDataType;
begin
  result := TUMLDataType(InternalAddNew);
end;

procedure TUMLDataTypeList.Insert(index: Integer; NewObject: TUMLDataType);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLDataTypeList.GetBoldObject(index: Integer): TUMLDataType;
begin
  result := TUMLDataType(GetElement(index));
end;

procedure TUMLDataTypeList.SetBoldObject(index: Integer; NewObject: TUMLDataType);
begin;
  SetElement(index, NewObject);
end;

{ TUMLInterface }

procedure TUMLInterfaceList.Add(NewObject: TUMLInterface);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLInterfaceList.IndexOf(anObject: TUMLInterface): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLInterfaceList.Includes(anObject: TUMLInterface) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLInterfaceList.AddNew: TUMLInterface;
begin
  result := TUMLInterface(InternalAddNew);
end;

procedure TUMLInterfaceList.Insert(index: Integer; NewObject: TUMLInterface);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLInterfaceList.GetBoldObject(index: Integer): TUMLInterface;
begin
  result := TUMLInterface(GetElement(index));
end;

procedure TUMLInterfaceList.SetBoldObject(index: Integer; NewObject: TUMLInterface);
begin;
  SetElement(index, NewObject);
end;

{ TUMLNode }

function TUMLNode._Getresident_: TUMLComponentList;
begin
  assert(ValidateMember('TUMLNode', 'resident_', 68, TUMLComponentList));
  Result := TUMLComponentList(BoldMembers[68]);
end;

function TUMLNode._GetdeploymentLocationresident: TdeploymentLocationresidentList;
begin
  assert(ValidateMember('TUMLNode', 'deploymentLocationresident', 69, TdeploymentLocationresidentList));
  Result := TdeploymentLocationresidentList(BoldMembers[69]);
end;

procedure TUMLNodeList.Add(NewObject: TUMLNode);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLNodeList.IndexOf(anObject: TUMLNode): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLNodeList.Includes(anObject: TUMLNode) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLNodeList.AddNew: TUMLNode;
begin
  result := TUMLNode(InternalAddNew);
end;

procedure TUMLNodeList.Insert(index: Integer; NewObject: TUMLNode);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLNodeList.GetBoldObject(index: Integer): TUMLNode;
begin
  result := TUMLNode(GetElement(index));
end;

procedure TUMLNodeList.SetBoldObject(index: Integer; NewObject: TUMLNode);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSignal }

function TUMLSignal._GetsendAction: TUMLSendActionList;
begin
  assert(ValidateMember('TUMLSignal', 'sendAction', 68, TUMLSendActionList));
  Result := TUMLSendActionList(BoldMembers[68]);
end;

function TUMLSignal._Getoccurrences: TUMLSignalEventList;
begin
  assert(ValidateMember('TUMLSignal', 'occurrences', 69, TUMLSignalEventList));
  Result := TUMLSignalEventList(BoldMembers[69]);
end;

function TUMLSignal._Getreception: TUMLReceptionList;
begin
  assert(ValidateMember('TUMLSignal', 'reception', 70, TUMLReceptionList));
  Result := TUMLReceptionList(BoldMembers[70]);
end;

function TUMLSignal._Getcontext: TUMLBehavioralFeatureList;
begin
  assert(ValidateMember('TUMLSignal', 'context', 71, TUMLBehavioralFeatureList));
  Result := TUMLBehavioralFeatureList(BoldMembers[71]);
end;

function TUMLSignal._GetcontextraisedSignal: TcontextraisedSignalList;
begin
  assert(ValidateMember('TUMLSignal', 'contextraisedSignal', 72, TcontextraisedSignalList));
  Result := TcontextraisedSignalList(BoldMembers[72]);
end;

procedure TUMLSignalList.Add(NewObject: TUMLSignal);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSignalList.IndexOf(anObject: TUMLSignal): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSignalList.Includes(anObject: TUMLSignal) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSignalList.AddNew: TUMLSignal;
begin
  result := TUMLSignal(InternalAddNew);
end;

procedure TUMLSignalList.Insert(index: Integer; NewObject: TUMLSignal);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSignalList.GetBoldObject(index: Integer): TUMLSignal;
begin
  result := TUMLSignal(GetElement(index));
end;

procedure TUMLSignalList.SetBoldObject(index: Integer; NewObject: TUMLSignal);
begin;
  SetElement(index, NewObject);
end;

{ TUMLUseCase }

function TUMLUseCase._Getinclude: TUMLIncludeList;
begin
  assert(ValidateMember('TUMLUseCase', 'include', 68, TUMLIncludeList));
  Result := TUMLIncludeList(BoldMembers[68]);
end;

function TUMLUseCase._GetextensionPoint: TUMLExtensionPointList;
begin
  assert(ValidateMember('TUMLUseCase', 'extensionPoint', 69, TUMLExtensionPointList));
  Result := TUMLExtensionPointList(BoldMembers[69]);
end;

function TUMLUseCase._Getextend2: TUMLExtendList;
begin
  assert(ValidateMember('TUMLUseCase', 'extend2', 70, TUMLExtendList));
  Result := TUMLExtendList(BoldMembers[70]);
end;

function TUMLUseCase._Getextend: TUMLExtendList;
begin
  assert(ValidateMember('TUMLUseCase', 'extend', 71, TUMLExtendList));
  Result := TUMLExtendList(BoldMembers[71]);
end;

function TUMLUseCase._Getinclude2: TUMLIncludeList;
begin
  assert(ValidateMember('TUMLUseCase', 'include2', 72, TUMLIncludeList));
  Result := TUMLIncludeList(BoldMembers[72]);
end;

procedure TUMLUseCaseList.Add(NewObject: TUMLUseCase);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLUseCaseList.IndexOf(anObject: TUMLUseCase): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLUseCaseList.Includes(anObject: TUMLUseCase) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLUseCaseList.AddNew: TUMLUseCase;
begin
  result := TUMLUseCase(InternalAddNew);
end;

procedure TUMLUseCaseList.Insert(index: Integer; NewObject: TUMLUseCase);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLUseCaseList.GetBoldObject(index: Integer): TUMLUseCase;
begin
  result := TUMLUseCase(GetElement(index));
end;

procedure TUMLUseCaseList.SetBoldObject(index: Integer; NewObject: TUMLUseCase);
begin;
  SetElement(index, NewObject);
end;

{ TUMLModel }

function TUMLModel._Get_M_Validator: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLModel', 'Validator', 50, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[50]);
end;

function TUMLModel._GetValidator: TValidator;
begin
  Result := TValidator(M_Validator.BoldObject);
  assert(not assigned(Result) or (Result is TValidator), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Validator', Result.ClassName, 'TValidator']));
end;

procedure TUMLModel._SetValidator(const value: TValidator);
begin
  M_Validator.BoldObject := value;
end;

procedure TUMLModelList.Add(NewObject: TUMLModel);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLModelList.IndexOf(anObject: TUMLModel): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLModelList.Includes(anObject: TUMLModel) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLModelList.AddNew: TUMLModel;
begin
  result := TUMLModel(InternalAddNew);
end;

procedure TUMLModelList.Insert(index: Integer; NewObject: TUMLModel);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLModelList.GetBoldObject(index: Integer): TUMLModel;
begin
  result := TUMLModel(GetElement(index));
end;

procedure TUMLModelList.SetBoldObject(index: Integer; NewObject: TUMLModel);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSubsystem }

function TUMLSubsystem._Get_M_isInstantiable: TBABoolean;
begin
  assert(ValidateMember('TUMLSubsystem', 'isInstantiable', 50, TBABoolean));
  Result := TBABoolean(BoldMembers[50]);
end;

function TUMLSubsystem._GetisInstantiable: boolean;
begin
  Result := M_isInstantiable.AsBoolean;
end;

procedure TUMLSubsystem._SetisInstantiable(const NewValue: boolean);
begin
  M_isInstantiable.AsBoolean := NewValue;
end;

procedure TUMLSubsystemList.Add(NewObject: TUMLSubsystem);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSubsystemList.IndexOf(anObject: TUMLSubsystem): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSubsystemList.Includes(anObject: TUMLSubsystem) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSubsystemList.AddNew: TUMLSubsystem;
begin
  result := TUMLSubsystem(InternalAddNew);
end;

procedure TUMLSubsystemList.Insert(index: Integer; NewObject: TUMLSubsystem);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSubsystemList.GetBoldObject(index: Integer): TUMLSubsystem;
begin
  result := TUMLSubsystem(GetElement(index));
end;

procedure TUMLSubsystemList.SetBoldObject(index: Integer; NewObject: TUMLSubsystem);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSubmachineState }

function TUMLSubmachineState._Get_M_submachine: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLSubmachineState', 'submachine', 54, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[54]);
end;

function TUMLSubmachineState._Getsubmachine: TUMLStateMachine;
begin
  Result := TUMLStateMachine(M_submachine.BoldObject);
  assert(not assigned(Result) or (Result is TUMLStateMachine), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'submachine', Result.ClassName, 'TUMLStateMachine']));
end;

procedure TUMLSubmachineState._Setsubmachine(const value: TUMLStateMachine);
begin
  M_submachine.BoldObject := value;
end;

procedure TUMLSubmachineStateList.Add(NewObject: TUMLSubmachineState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSubmachineStateList.IndexOf(anObject: TUMLSubmachineState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSubmachineStateList.Includes(anObject: TUMLSubmachineState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSubmachineStateList.AddNew: TUMLSubmachineState;
begin
  result := TUMLSubmachineState(InternalAddNew);
end;

procedure TUMLSubmachineStateList.Insert(index: Integer; NewObject: TUMLSubmachineState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSubmachineStateList.GetBoldObject(index: Integer): TUMLSubmachineState;
begin
  result := TUMLSubmachineState(GetElement(index));
end;

procedure TUMLSubmachineStateList.SetBoldObject(index: Integer; NewObject: TUMLSubmachineState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLActionState }

function TUMLActionState._Get_M_isDynamic: TBABoolean;
begin
  assert(ValidateMember('TUMLActionState', 'isDynamic', 52, TBABoolean));
  Result := TBABoolean(BoldMembers[52]);
end;

function TUMLActionState._GetisDynamic: boolean;
begin
  Result := M_isDynamic.AsBoolean;
end;

procedure TUMLActionState._SetisDynamic(const NewValue: boolean);
begin
  M_isDynamic.AsBoolean := NewValue;
end;

function TUMLActionState._Get_M_dynamicArguments: TBAString;
begin
  assert(ValidateMember('TUMLActionState', 'dynamicArguments', 53, TBAString));
  Result := TBAString(BoldMembers[53]);
end;

function TUMLActionState._GetdynamicArguments: String;
begin
  Result := M_dynamicArguments.AsString;
end;

procedure TUMLActionState._SetdynamicArguments(const NewValue: String);
begin
  M_dynamicArguments.AsString := NewValue;
end;

function TUMLActionState._Get_M_dynamicMultiplicity: TBAString;
begin
  assert(ValidateMember('TUMLActionState', 'dynamicMultiplicity', 54, TBAString));
  Result := TBAString(BoldMembers[54]);
end;

function TUMLActionState._GetdynamicMultiplicity: String;
begin
  Result := M_dynamicMultiplicity.AsString;
end;

procedure TUMLActionState._SetdynamicMultiplicity(const NewValue: String);
begin
  M_dynamicMultiplicity.AsString := NewValue;
end;

procedure TUMLActionStateList.Add(NewObject: TUMLActionState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLActionStateList.IndexOf(anObject: TUMLActionState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLActionStateList.Includes(anObject: TUMLActionState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLActionStateList.AddNew: TUMLActionState;
begin
  result := TUMLActionState(InternalAddNew);
end;

procedure TUMLActionStateList.Insert(index: Integer; NewObject: TUMLActionState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLActionStateList.GetBoldObject(index: Integer): TUMLActionState;
begin
  result := TUMLActionState(GetElement(index));
end;

procedure TUMLActionStateList.SetBoldObject(index: Integer; NewObject: TUMLActionState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLObjectFlowState }

function TUMLObjectFlowState._Get_M_isSynch: TBABoolean;
begin
  assert(ValidateMember('TUMLObjectFlowState', 'isSynch', 52, TBABoolean));
  Result := TBABoolean(BoldMembers[52]);
end;

function TUMLObjectFlowState._GetisSynch: boolean;
begin
  Result := M_isSynch.AsBoolean;
end;

procedure TUMLObjectFlowState._SetisSynch(const NewValue: boolean);
begin
  M_isSynch.AsBoolean := NewValue;
end;

function TUMLObjectFlowState._Get_M_type_: TBoldObjectReference;
begin
  assert(ValidateMember('TUMLObjectFlowState', 'type_', 53, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[53]);
end;

function TUMLObjectFlowState._Gettype_: TUMLClassifier;
begin
  Result := TUMLClassifier(M_type_.BoldObject);
  assert(not assigned(Result) or (Result is TUMLClassifier), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'type_', Result.ClassName, 'TUMLClassifier']));
end;

procedure TUMLObjectFlowState._Settype_(const value: TUMLClassifier);
begin
  M_type_.BoldObject := value;
end;

function TUMLObjectFlowState._Getparameter: TUMLParameterList;
begin
  assert(ValidateMember('TUMLObjectFlowState', 'parameter', 54, TUMLParameterList));
  Result := TUMLParameterList(BoldMembers[54]);
end;

function TUMLObjectFlowState._Getparameterstate: TparameterstateList;
begin
  assert(ValidateMember('TUMLObjectFlowState', 'parameterstate', 55, TparameterstateList));
  Result := TparameterstateList(BoldMembers[55]);
end;

procedure TUMLObjectFlowStateList.Add(NewObject: TUMLObjectFlowState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLObjectFlowStateList.IndexOf(anObject: TUMLObjectFlowState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLObjectFlowStateList.Includes(anObject: TUMLObjectFlowState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLObjectFlowStateList.AddNew: TUMLObjectFlowState;
begin
  result := TUMLObjectFlowState(InternalAddNew);
end;

procedure TUMLObjectFlowStateList.Insert(index: Integer; NewObject: TUMLObjectFlowState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLObjectFlowStateList.GetBoldObject(index: Integer): TUMLObjectFlowState;
begin
  result := TUMLObjectFlowState(GetElement(index));
end;

procedure TUMLObjectFlowStateList.SetBoldObject(index: Integer; NewObject: TUMLObjectFlowState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLException }

procedure TUMLExceptionList.Add(NewObject: TUMLException);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLExceptionList.IndexOf(anObject: TUMLException): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLExceptionList.Includes(anObject: TUMLException) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLExceptionList.AddNew: TUMLException;
begin
  result := TUMLException(InternalAddNew);
end;

procedure TUMLExceptionList.Insert(index: Integer; NewObject: TUMLException);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLExceptionList.GetBoldObject(index: Integer): TUMLException;
begin
  result := TUMLException(GetElement(index));
end;

procedure TUMLExceptionList.SetBoldObject(index: Integer; NewObject: TUMLException);
begin;
  SetElement(index, NewObject);
end;

{ TUMLSubactivityState }

function TUMLSubactivityState._Get_M_isDynamic: TBABoolean;
begin
  assert(ValidateMember('TUMLSubactivityState', 'isDynamic', 55, TBABoolean));
  Result := TBABoolean(BoldMembers[55]);
end;

function TUMLSubactivityState._GetisDynamic: boolean;
begin
  Result := M_isDynamic.AsBoolean;
end;

procedure TUMLSubactivityState._SetisDynamic(const NewValue: boolean);
begin
  M_isDynamic.AsBoolean := NewValue;
end;

function TUMLSubactivityState._Get_M_dynamicArguments: TBAString;
begin
  assert(ValidateMember('TUMLSubactivityState', 'dynamicArguments', 56, TBAString));
  Result := TBAString(BoldMembers[56]);
end;

function TUMLSubactivityState._GetdynamicArguments: String;
begin
  Result := M_dynamicArguments.AsString;
end;

procedure TUMLSubactivityState._SetdynamicArguments(const NewValue: String);
begin
  M_dynamicArguments.AsString := NewValue;
end;

function TUMLSubactivityState._Get_M_dynamicMultiplicity: TBAString;
begin
  assert(ValidateMember('TUMLSubactivityState', 'dynamicMultiplicity', 57, TBAString));
  Result := TBAString(BoldMembers[57]);
end;

function TUMLSubactivityState._GetdynamicMultiplicity: String;
begin
  Result := M_dynamicMultiplicity.AsString;
end;

procedure TUMLSubactivityState._SetdynamicMultiplicity(const NewValue: String);
begin
  M_dynamicMultiplicity.AsString := NewValue;
end;

procedure TUMLSubactivityStateList.Add(NewObject: TUMLSubactivityState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLSubactivityStateList.IndexOf(anObject: TUMLSubactivityState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLSubactivityStateList.Includes(anObject: TUMLSubactivityState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLSubactivityStateList.AddNew: TUMLSubactivityState;
begin
  result := TUMLSubactivityState(InternalAddNew);
end;

procedure TUMLSubactivityStateList.Insert(index: Integer; NewObject: TUMLSubactivityState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLSubactivityStateList.GetBoldObject(index: Integer): TUMLSubactivityState;
begin
  result := TUMLSubactivityState(GetElement(index));
end;

procedure TUMLSubactivityStateList.SetBoldObject(index: Integer; NewObject: TUMLSubactivityState);
begin;
  SetElement(index, NewObject);
end;

{ TUMLCallState }

procedure TUMLCallStateList.Add(NewObject: TUMLCallState);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TUMLCallStateList.IndexOf(anObject: TUMLCallState): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TUMLCallStateList.Includes(anObject: TUMLCallState) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TUMLCallStateList.AddNew: TUMLCallState;
begin
  result := TUMLCallState(InternalAddNew);
end;

procedure TUMLCallStateList.Insert(index: Integer; NewObject: TUMLCallState);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TUMLCallStateList.GetBoldObject(index: Integer): TUMLCallState;
begin
  result := TUMLCallState(GetElement(index));
end;

procedure TUMLCallStateList.SetBoldObject(index: Integer; NewObject: TUMLCallState);
begin;
  SetElement(index, NewObject);
end;

function GeneratedCodeCRC: String;
begin
  result := '673100103';
end;

procedure InstallObjectListClasses(BoldObjectListClasses: TBoldGeneratedClassList);
begin
  BoldObjectListClasses.AddObjectEntry('UMLModelRoot', TUMLModelRootList);
  BoldObjectListClasses.AddObjectEntry('Argumentstimulus1', Targumentstimulus1List);
  BoldObjectListClasses.AddObjectEntry('AssociationEndRoleavailableQualifier', TassociationEndRoleavailableQualifierList);
  BoldObjectListClasses.AddObjectEntry('ClassifierclassifierRole_', TclassifierclassifierRole_List);
  BoldObjectListClasses.AddObjectEntry('ClassifierInStateinState', TclassifierInStateinStateList);
  BoldObjectListClasses.AddObjectEntry('ClassifierRole_availableFeature', TclassifierRole_availableFeatureList);
  BoldObjectListClasses.AddObjectEntry('ClassifierRoleavailableContents', TclassifierRoleavailableContentsList);
  BoldObjectListClasses.AddObjectEntry('ClientclientDependency', TclientclientDependencyList);
  BoldObjectListClasses.AddObjectEntry('CollaborationconstrainingElement', TcollaborationconstrainingElementList);
  BoldObjectListClasses.AddObjectEntry('CommentannotatedElement', TcommentannotatedElementList);
  BoldObjectListClasses.AddObjectEntry('ConstrainedElementconstraint', TconstrainedElementconstraintList);
  BoldObjectListClasses.AddObjectEntry('Contentspartition', TcontentspartitionList);
  BoldObjectListClasses.AddObjectEntry('ContextraisedSignal', TcontextraisedSignalList);
  BoldObjectListClasses.AddObjectEntry('DeploymentLocationresident', TdeploymentLocationresidentList);
  BoldObjectListClasses.AddObjectEntry('UMLElement', TUMLElementList);
  BoldObjectListClasses.AddObjectEntry('UMLElementImport', TUMLElementImportList);
  BoldObjectListClasses.AddObjectEntry('UMLElementResidence', TUMLElementResidenceList);
  BoldObjectListClasses.AddObjectEntry('ExtensionPointextend', TextensionPointextendList);
  BoldObjectListClasses.AddObjectEntry('Instanceclassifier', TinstanceclassifierList);
  BoldObjectListClasses.AddObjectEntry('Parameterstate', TparameterstateList);
  BoldObjectListClasses.AddObjectEntry('Participantspecification', TparticipantspecificationList);
  BoldObjectListClasses.AddObjectEntry('Predecessormessage3', Tpredecessormessage3List);
  BoldObjectListClasses.AddObjectEntry('UMLPresentationElement', TUMLPresentationElementList);
  BoldObjectListClasses.AddObjectEntry('Presentationsubject', TpresentationsubjectList);
  BoldObjectListClasses.AddObjectEntry('SourceFlowsource', TsourceFlowsourceList);
  BoldObjectListClasses.AddObjectEntry('StatedeferrableEvent', TstatedeferrableEventList);
  BoldObjectListClasses.AddObjectEntry('SuppliersupplierDependency', TsuppliersupplierDependencyList);
  BoldObjectListClasses.AddObjectEntry('TargetFlowtarget', TtargetFlowtargetList);
  BoldObjectListClasses.AddObjectEntry('UMLTemplateParameter', TUMLTemplateParameterList);
  BoldObjectListClasses.AddObjectEntry('Validator', TValidatorList);
  BoldObjectListClasses.AddObjectEntry('Violation', TViolationList);
  BoldObjectListClasses.AddObjectEntry('UMLModelElement', TUMLModelElementList);
  BoldObjectListClasses.AddObjectEntry('UMLAction', TUMLActionList);
  BoldObjectListClasses.AddObjectEntry('UMLArgument', TUMLArgumentList);
  BoldObjectListClasses.AddObjectEntry('UMLAssociationEnd', TUMLAssociationEndList);
  BoldObjectListClasses.AddObjectEntry('UMLAttributeLink', TUMLAttributeLinkList);
  BoldObjectListClasses.AddObjectEntry('UMLComment', TUMLCommentList);
  BoldObjectListClasses.AddObjectEntry('UMLConstraint', TUMLConstraintList);
  BoldObjectListClasses.AddObjectEntry('UMLEvent', TUMLEventList);
  BoldObjectListClasses.AddObjectEntry('UMLExtensionPoint', TUMLExtensionPointList);
  BoldObjectListClasses.AddObjectEntry('UMLFeature', TUMLFeatureList);
  BoldObjectListClasses.AddObjectEntry('UMLGuard', TUMLGuardList);
  BoldObjectListClasses.AddObjectEntry('UMLInstance', TUMLInstanceList);
  BoldObjectListClasses.AddObjectEntry('UMLInteraction', TUMLInteractionList);
  BoldObjectListClasses.AddObjectEntry('UMLLink', TUMLLinkList);
  BoldObjectListClasses.AddObjectEntry('UMLLinkEnd', TUMLLinkEndList);
  BoldObjectListClasses.AddObjectEntry('UMLMessage', TUMLMessageList);
  BoldObjectListClasses.AddObjectEntry('UMLNamespace', TUMLNamespaceList);
  BoldObjectListClasses.AddObjectEntry('UMLParameter', TUMLParameterList);
  BoldObjectListClasses.AddObjectEntry('UMLPartition', TUMLPartitionList);
  BoldObjectListClasses.AddObjectEntry('UMLRelationship', TUMLRelationshipList);
  BoldObjectListClasses.AddObjectEntry('UMLStateMachine', TUMLStateMachineList);
  BoldObjectListClasses.AddObjectEntry('UMLStateVertex', TUMLStateVertexList);
  BoldObjectListClasses.AddObjectEntry('UMLStimulus', TUMLStimulusList);
  BoldObjectListClasses.AddObjectEntry('UMLTaggedValue', TUMLTaggedValueList);
  BoldObjectListClasses.AddObjectEntry('UMLTransition', TUMLTransitionList);
  BoldObjectListClasses.AddObjectEntry('UMLActionSequence', TUMLActionSequenceList);
  BoldObjectListClasses.AddObjectEntry('UMLCallAction', TUMLCallActionList);
  BoldObjectListClasses.AddObjectEntry('UMLCreateAction', TUMLCreateActionList);
  BoldObjectListClasses.AddObjectEntry('UMLDestroyAction', TUMLDestroyActionList);
  BoldObjectListClasses.AddObjectEntry('UMLReturnAction', TUMLReturnActionList);
  BoldObjectListClasses.AddObjectEntry('UMLSendAction', TUMLSendActionList);
  BoldObjectListClasses.AddObjectEntry('UMLTerminateAction', TUMLTerminateActionList);
  BoldObjectListClasses.AddObjectEntry('UMLUninterpretedAction', TUMLUninterpretedActionList);
  BoldObjectListClasses.AddObjectEntry('UMLAssociationEndRole', TUMLAssociationEndRoleList);
  BoldObjectListClasses.AddObjectEntry('UMLCallEvent', TUMLCallEventList);
  BoldObjectListClasses.AddObjectEntry('UMLChangeEvent', TUMLChangeEventList);
  BoldObjectListClasses.AddObjectEntry('UMLSignalEvent', TUMLSignalEventList);
  BoldObjectListClasses.AddObjectEntry('UMLTimeEvent', TUMLTimeEventList);
  BoldObjectListClasses.AddObjectEntry('UMLBehavioralFeature', TUMLBehavioralFeatureList);
  BoldObjectListClasses.AddObjectEntry('UMLStructuralFeature', TUMLStructuralFeatureList);
  BoldObjectListClasses.AddObjectEntry('UMLComponentInstance', TUMLComponentInstanceList);
  BoldObjectListClasses.AddObjectEntry('UMLDataValue', TUMLDataValueList);
  BoldObjectListClasses.AddObjectEntry('UMLNodeInstance', TUMLNodeInstanceList);
  BoldObjectListClasses.AddObjectEntry('UMLObject', TUMLObjectList);
  BoldObjectListClasses.AddObjectEntry('UMLUseCaseInstance', TUMLUseCaseInstanceList);
  BoldObjectListClasses.AddObjectEntry('UMLGeneralizableElement', TUMLGeneralizableElementList);
  BoldObjectListClasses.AddObjectEntry('UMLDependency', TUMLDependencyList);
  BoldObjectListClasses.AddObjectEntry('UMLExtend', TUMLExtendList);
  BoldObjectListClasses.AddObjectEntry('UMLFlow', TUMLFlowList);
  BoldObjectListClasses.AddObjectEntry('UMLGeneralization', TUMLGeneralizationList);
  BoldObjectListClasses.AddObjectEntry('UMLInclude', TUMLIncludeList);
  BoldObjectListClasses.AddObjectEntry('UMLActivityGraph', TUMLActivityGraphList);
  BoldObjectListClasses.AddObjectEntry('UMLPseudostate', TUMLPseudostateList);
  BoldObjectListClasses.AddObjectEntry('UMLState', TUMLStateList);
  BoldObjectListClasses.AddObjectEntry('UMLStubState', TUMLStubStateList);
  BoldObjectListClasses.AddObjectEntry('UMLSynchState', TUMLSynchStateList);
  BoldObjectListClasses.AddObjectEntry('UMLMethod', TUMLMethodList);
  BoldObjectListClasses.AddObjectEntry('UMLOperation', TUMLOperationList);
  BoldObjectListClasses.AddObjectEntry('UMLReception', TUMLReceptionList);
  BoldObjectListClasses.AddObjectEntry('UMLAttribute', TUMLAttributeList);
  BoldObjectListClasses.AddObjectEntry('UMLAssociation', TUMLAssociationList);
  BoldObjectListClasses.AddObjectEntry('UMLClassifier', TUMLClassifierList);
  BoldObjectListClasses.AddObjectEntry('UMLCollaboration', TUMLCollaborationList);
  BoldObjectListClasses.AddObjectEntry('UMLPackage', TUMLPackageList);
  BoldObjectListClasses.AddObjectEntry('UMLStereotype', TUMLStereotypeList);
  BoldObjectListClasses.AddObjectEntry('UMLAbstraction', TUMLAbstractionList);
  BoldObjectListClasses.AddObjectEntry('UMLBinding', TUMLBindingList);
  BoldObjectListClasses.AddObjectEntry('UMLPermission', TUMLPermissionList);
  BoldObjectListClasses.AddObjectEntry('UMLUsage', TUMLUsageList);
  BoldObjectListClasses.AddObjectEntry('UMLCompositeState', TUMLCompositeStateList);
  BoldObjectListClasses.AddObjectEntry('UMLFinalState', TUMLFinalStateList);
  BoldObjectListClasses.AddObjectEntry('UMLSimpleState', TUMLSimpleStateList);
  BoldObjectListClasses.AddObjectEntry('UMLAssociationRole', TUMLAssociationRoleList);
  BoldObjectListClasses.AddObjectEntry('UMLActor', TUMLActorList);
  BoldObjectListClasses.AddObjectEntry('UMLClass', TUMLClassList);
  BoldObjectListClasses.AddObjectEntry('UMLClassifierInState', TUMLClassifierInStateList);
  BoldObjectListClasses.AddObjectEntry('UMLClassifierRole', TUMLClassifierRoleList);
  BoldObjectListClasses.AddObjectEntry('UMLComponent', TUMLComponentList);
  BoldObjectListClasses.AddObjectEntry('UMLDataType', TUMLDataTypeList);
  BoldObjectListClasses.AddObjectEntry('UMLInterface', TUMLInterfaceList);
  BoldObjectListClasses.AddObjectEntry('UMLNode', TUMLNodeList);
  BoldObjectListClasses.AddObjectEntry('UMLSignal', TUMLSignalList);
  BoldObjectListClasses.AddObjectEntry('UMLUseCase', TUMLUseCaseList);
  BoldObjectListClasses.AddObjectEntry('UMLModel', TUMLModelList);
  BoldObjectListClasses.AddObjectEntry('UMLSubsystem', TUMLSubsystemList);
  BoldObjectListClasses.AddObjectEntry('UMLSubmachineState', TUMLSubmachineStateList);
  BoldObjectListClasses.AddObjectEntry('UMLActionState', TUMLActionStateList);
  BoldObjectListClasses.AddObjectEntry('UMLObjectFlowState', TUMLObjectFlowStateList);
  BoldObjectListClasses.AddObjectEntry('UMLException', TUMLExceptionList);
  BoldObjectListClasses.AddObjectEntry('UMLSubactivityState', TUMLSubactivityStateList);
  BoldObjectListClasses.AddObjectEntry('UMLCallState', TUMLCallStateList);
end;

procedure InstallBusinessClasses(BoldObjectClasses: TBoldGeneratedClassList);
begin
  BoldObjectClasses.AddObjectEntry('UMLModelRoot', TUMLModelRoot);
  BoldObjectClasses.AddObjectEntry('Argumentstimulus1', Targumentstimulus1);
  BoldObjectClasses.AddObjectEntry('AssociationEndRoleavailableQualifier', TassociationEndRoleavailableQualifier);
  BoldObjectClasses.AddObjectEntry('ClassifierclassifierRole_', TclassifierclassifierRole_);
  BoldObjectClasses.AddObjectEntry('ClassifierInStateinState', TclassifierInStateinState);
  BoldObjectClasses.AddObjectEntry('ClassifierRole_availableFeature', TclassifierRole_availableFeature);
  BoldObjectClasses.AddObjectEntry('ClassifierRoleavailableContents', TclassifierRoleavailableContents);
  BoldObjectClasses.AddObjectEntry('ClientclientDependency', TclientclientDependency);
  BoldObjectClasses.AddObjectEntry('CollaborationconstrainingElement', TcollaborationconstrainingElement);
  BoldObjectClasses.AddObjectEntry('CommentannotatedElement', TcommentannotatedElement);
  BoldObjectClasses.AddObjectEntry('ConstrainedElementconstraint', TconstrainedElementconstraint);
  BoldObjectClasses.AddObjectEntry('Contentspartition', Tcontentspartition);
  BoldObjectClasses.AddObjectEntry('ContextraisedSignal', TcontextraisedSignal);
  BoldObjectClasses.AddObjectEntry('DeploymentLocationresident', TdeploymentLocationresident);
  BoldObjectClasses.AddObjectEntry('UMLElement', TUMLElement);
  BoldObjectClasses.AddObjectEntry('UMLElementImport', TUMLElementImport);
  BoldObjectClasses.AddObjectEntry('UMLElementResidence', TUMLElementResidence);
  BoldObjectClasses.AddObjectEntry('ExtensionPointextend', TextensionPointextend);
  BoldObjectClasses.AddObjectEntry('Instanceclassifier', Tinstanceclassifier);
  BoldObjectClasses.AddObjectEntry('Parameterstate', Tparameterstate);
  BoldObjectClasses.AddObjectEntry('Participantspecification', Tparticipantspecification);
  BoldObjectClasses.AddObjectEntry('Predecessormessage3', Tpredecessormessage3);
  BoldObjectClasses.AddObjectEntry('UMLPresentationElement', TUMLPresentationElement);
  BoldObjectClasses.AddObjectEntry('Presentationsubject', Tpresentationsubject);
  BoldObjectClasses.AddObjectEntry('SourceFlowsource', TsourceFlowsource);
  BoldObjectClasses.AddObjectEntry('StatedeferrableEvent', TstatedeferrableEvent);
  BoldObjectClasses.AddObjectEntry('SuppliersupplierDependency', TsuppliersupplierDependency);
  BoldObjectClasses.AddObjectEntry('TargetFlowtarget', TtargetFlowtarget);
  BoldObjectClasses.AddObjectEntry('UMLTemplateParameter', TUMLTemplateParameter);
  BoldObjectClasses.AddObjectEntry('Validator', TValidator);
  BoldObjectClasses.AddObjectEntry('Violation', TViolation);
  BoldObjectClasses.AddObjectEntry('UMLModelElement', TUMLModelElement);
  BoldObjectClasses.AddObjectEntry('UMLAction', TUMLAction);
  BoldObjectClasses.AddObjectEntry('UMLArgument', TUMLArgument);
  BoldObjectClasses.AddObjectEntry('UMLAssociationEnd', TUMLAssociationEnd);
  BoldObjectClasses.AddObjectEntry('UMLAttributeLink', TUMLAttributeLink);
  BoldObjectClasses.AddObjectEntry('UMLComment', TUMLComment);
  BoldObjectClasses.AddObjectEntry('UMLConstraint', TUMLConstraint);
  BoldObjectClasses.AddObjectEntry('UMLEvent', TUMLEvent);
  BoldObjectClasses.AddObjectEntry('UMLExtensionPoint', TUMLExtensionPoint);
  BoldObjectClasses.AddObjectEntry('UMLFeature', TUMLFeature);
  BoldObjectClasses.AddObjectEntry('UMLGuard', TUMLGuard);
  BoldObjectClasses.AddObjectEntry('UMLInstance', TUMLInstance);
  BoldObjectClasses.AddObjectEntry('UMLInteraction', TUMLInteraction);
  BoldObjectClasses.AddObjectEntry('UMLLink', TUMLLink);
  BoldObjectClasses.AddObjectEntry('UMLLinkEnd', TUMLLinkEnd);
  BoldObjectClasses.AddObjectEntry('UMLMessage', TUMLMessage);
  BoldObjectClasses.AddObjectEntry('UMLNamespace', TUMLNamespace);
  BoldObjectClasses.AddObjectEntry('UMLParameter', TUMLParameter);
  BoldObjectClasses.AddObjectEntry('UMLPartition', TUMLPartition);
  BoldObjectClasses.AddObjectEntry('UMLRelationship', TUMLRelationship);
  BoldObjectClasses.AddObjectEntry('UMLStateMachine', TUMLStateMachine);
  BoldObjectClasses.AddObjectEntry('UMLStateVertex', TUMLStateVertex);
  BoldObjectClasses.AddObjectEntry('UMLStimulus', TUMLStimulus);
  BoldObjectClasses.AddObjectEntry('UMLTaggedValue', TUMLTaggedValue);
  BoldObjectClasses.AddObjectEntry('UMLTransition', TUMLTransition);
  BoldObjectClasses.AddObjectEntry('UMLActionSequence', TUMLActionSequence);
  BoldObjectClasses.AddObjectEntry('UMLCallAction', TUMLCallAction);
  BoldObjectClasses.AddObjectEntry('UMLCreateAction', TUMLCreateAction);
  BoldObjectClasses.AddObjectEntry('UMLDestroyAction', TUMLDestroyAction);
  BoldObjectClasses.AddObjectEntry('UMLReturnAction', TUMLReturnAction);
  BoldObjectClasses.AddObjectEntry('UMLSendAction', TUMLSendAction);
  BoldObjectClasses.AddObjectEntry('UMLTerminateAction', TUMLTerminateAction);
  BoldObjectClasses.AddObjectEntry('UMLUninterpretedAction', TUMLUninterpretedAction);
  BoldObjectClasses.AddObjectEntry('UMLAssociationEndRole', TUMLAssociationEndRole);
  BoldObjectClasses.AddObjectEntry('UMLCallEvent', TUMLCallEvent);
  BoldObjectClasses.AddObjectEntry('UMLChangeEvent', TUMLChangeEvent);
  BoldObjectClasses.AddObjectEntry('UMLSignalEvent', TUMLSignalEvent);
  BoldObjectClasses.AddObjectEntry('UMLTimeEvent', TUMLTimeEvent);
  BoldObjectClasses.AddObjectEntry('UMLBehavioralFeature', TUMLBehavioralFeature);
  BoldObjectClasses.AddObjectEntry('UMLStructuralFeature', TUMLStructuralFeature);
  BoldObjectClasses.AddObjectEntry('UMLComponentInstance', TUMLComponentInstance);
  BoldObjectClasses.AddObjectEntry('UMLDataValue', TUMLDataValue);
  BoldObjectClasses.AddObjectEntry('UMLNodeInstance', TUMLNodeInstance);
  BoldObjectClasses.AddObjectEntry('UMLObject', TUMLObject);
  BoldObjectClasses.AddObjectEntry('UMLUseCaseInstance', TUMLUseCaseInstance);
  BoldObjectClasses.AddObjectEntry('UMLGeneralizableElement', TUMLGeneralizableElement);
  BoldObjectClasses.AddObjectEntry('UMLDependency', TUMLDependency);
  BoldObjectClasses.AddObjectEntry('UMLExtend', TUMLExtend);
  BoldObjectClasses.AddObjectEntry('UMLFlow', TUMLFlow);
  BoldObjectClasses.AddObjectEntry('UMLGeneralization', TUMLGeneralization);
  BoldObjectClasses.AddObjectEntry('UMLInclude', TUMLInclude);
  BoldObjectClasses.AddObjectEntry('UMLActivityGraph', TUMLActivityGraph);
  BoldObjectClasses.AddObjectEntry('UMLPseudostate', TUMLPseudostate);
  BoldObjectClasses.AddObjectEntry('UMLState', TUMLState);
  BoldObjectClasses.AddObjectEntry('UMLStubState', TUMLStubState);
  BoldObjectClasses.AddObjectEntry('UMLSynchState', TUMLSynchState);
  BoldObjectClasses.AddObjectEntry('UMLMethod', TUMLMethod);
  BoldObjectClasses.AddObjectEntry('UMLOperation', TUMLOperation);
  BoldObjectClasses.AddObjectEntry('UMLReception', TUMLReception);
  BoldObjectClasses.AddObjectEntry('UMLAttribute', TUMLAttribute);
  BoldObjectClasses.AddObjectEntry('UMLAssociation', TUMLAssociation);
  BoldObjectClasses.AddObjectEntry('UMLClassifier', TUMLClassifier);
  BoldObjectClasses.AddObjectEntry('UMLCollaboration', TUMLCollaboration);
  BoldObjectClasses.AddObjectEntry('UMLPackage', TUMLPackage);
  BoldObjectClasses.AddObjectEntry('UMLStereotype', TUMLStereotype);
  BoldObjectClasses.AddObjectEntry('UMLAbstraction', TUMLAbstraction);
  BoldObjectClasses.AddObjectEntry('UMLBinding', TUMLBinding);
  BoldObjectClasses.AddObjectEntry('UMLPermission', TUMLPermission);
  BoldObjectClasses.AddObjectEntry('UMLUsage', TUMLUsage);
  BoldObjectClasses.AddObjectEntry('UMLCompositeState', TUMLCompositeState);
  BoldObjectClasses.AddObjectEntry('UMLFinalState', TUMLFinalState);
  BoldObjectClasses.AddObjectEntry('UMLSimpleState', TUMLSimpleState);
  BoldObjectClasses.AddObjectEntry('UMLAssociationRole', TUMLAssociationRole);
  BoldObjectClasses.AddObjectEntry('UMLActor', TUMLActor);
  BoldObjectClasses.AddObjectEntry('UMLClass', TUMLClass);
  BoldObjectClasses.AddObjectEntry('UMLClassifierInState', TUMLClassifierInState);
  BoldObjectClasses.AddObjectEntry('UMLClassifierRole', TUMLClassifierRole);
  BoldObjectClasses.AddObjectEntry('UMLComponent', TUMLComponent);
  BoldObjectClasses.AddObjectEntry('UMLDataType', TUMLDataType);
  BoldObjectClasses.AddObjectEntry('UMLInterface', TUMLInterface);
  BoldObjectClasses.AddObjectEntry('UMLNode', TUMLNode);
  BoldObjectClasses.AddObjectEntry('UMLSignal', TUMLSignal);
  BoldObjectClasses.AddObjectEntry('UMLUseCase', TUMLUseCase);
  BoldObjectClasses.AddObjectEntry('UMLModel', TUMLModel);
  BoldObjectClasses.AddObjectEntry('UMLSubsystem', TUMLSubsystem);
  BoldObjectClasses.AddObjectEntry('UMLSubmachineState', TUMLSubmachineState);
  BoldObjectClasses.AddObjectEntry('UMLActionState', TUMLActionState);
  BoldObjectClasses.AddObjectEntry('UMLObjectFlowState', TUMLObjectFlowState);
  BoldObjectClasses.AddObjectEntry('UMLException', TUMLException);
  BoldObjectClasses.AddObjectEntry('UMLSubactivityState', TUMLSubactivityState);
  BoldObjectClasses.AddObjectEntry('UMLCallState', TUMLCallState);
end;

var
  CodeDescriptor: TBoldGeneratedCodeDescriptor;

initialization
  CodeDescriptor := GeneratedCodes.AddGeneratedCodeDescriptorWithFunc('BoldUMLModel', InstallBusinessClasses, InstallObjectListClasses, GeneratedCodeCRC);
finalization
  GeneratedCodes.Remove(CodeDescriptor);
end.
