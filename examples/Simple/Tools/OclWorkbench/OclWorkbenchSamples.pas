{$INCLUDE bold.inc}
{-----------------------------------------------------------------------------
  OclWorkbenchSamples

  Two things the workbench needs that Bold itself does not provide:

    1. A catalogue of ready-made OCL expressions for the shared DemoModel, so
       the workbench opens with something to run instead of an empty memo.
    2. A seeding routine, because OCL over an empty system is not convincing.

  Nothing here is required in order to use BoldOclPropEditor or BoldOCLExplorer.
  It only gives those units something interesting to chew on.
-----------------------------------------------------------------------------}
unit OclWorkbenchSamples;

interface

uses
  BoldSystem;

type
  { One catalogue entry. Category groups the entries in the tree, Expression is
    handed to the evaluator verbatim, Comment says what the reader should
    notice about it. }
  TOclSample = record
    Category: string;
    Expression: string;
    Comment: string;
  end;

function OclSampleCount: Integer;
function OclSample(AIndex: Integer): TOclSample;

{ True when the system holds no Person yet, so seeding is worthwhile. }
function SampleDataIsEmpty(ASystem: TBoldSystem): Boolean;

{ Creates a small but deliberately varied population: both building subclasses,
  an association class carrying attributes, people with and without a home,
  a project without a manager, tasks done and undone, and one building nobody
  owns. Does not save; the caller decides when to call UpdateDatabase. }
procedure CreateSampleData(ASystem: TBoldSystem);

{ Object counts for the status bar. }
function SampleDataSummary(ASystem: TBoldSystem): string;

implementation

uses
  System.SysUtils,
  BoldAttributes,
  DemoClasses;

const
  { Anchoring the project dates on a fixed year keeps the date samples
    reproducible from one run to the next. }
  cAnchorYear = 2026;

  cOclSamples: array[0..65] of TOclSample = (

    // -- Starting points ---------------------------------------------------
    (Category: '1 Starting points';
     Expression: 'Person.allInstances';
     Comment: 'The whole extent of a class. Every OCL session starts somewhere like this.'),
    (Category: '1 Starting points';
     Expression: 'Person.allInstances->size';
     Comment: 'A collection operation. Note the arrow: -> reaches collections, . reaches members.'),
    (Category: '1 Starting points';
     Expression: 'Person.allInstances->isEmpty';
     Comment: 'Returns a Boolean, so the result pane shows a scalar rather than a grid.'),
    (Category: '1 Starting points';
     Expression: 'Building.allInstances';
     Comment: 'Building is a superclass, so its extent already includes both subclasses.'),
    (Category: '1 Starting points';
     Expression: 'BusinessClassesRoot.allInstances->size';
     Comment: 'The model root. Counts every object of every class in one go.'),

    // -- Navigation --------------------------------------------------------
    (Category: '2 Navigation';
     Expression: 'Project.allInstances.manager';
     Comment: 'Navigating a single-valued role across a collection gathers the results.'),
    (Category: '2 Navigation';
     Expression: 'Project.allInstances.tasks';
     Comment: 'Navigating a multi-valued role flattens: a list of lists becomes one list.'),
    (Category: '2 Navigation';
     Expression: 'Person.allInstances.home';
     Comment: 'People with no home contribute nothing. Unset links drop out silently.'),
    (Category: '2 Navigation';
     Expression: 'Task.allInstances.project.manager.lastName';
     Comment: 'Three roles deep, ending in an attribute. No join written by hand.'),
    (Category: '2 Navigation';
     Expression: 'ResidentialBuilding.allInstances.residents';
     Comment: 'The other end of Person.home, declared once in the model as one association.'),

    // -- Filtering ---------------------------------------------------------
    (Category: '3 Filtering';
     Expression: 'Person.allInstances->select(isActive)';
     Comment: 'select keeps elements whose body is true. Here the body is just a Boolean attribute.'),
    (Category: '3 Filtering';
     Expression: 'Person.allInstances->reject(isActive)';
     Comment: 'reject is the complement of select.'),
    (Category: '3 Filtering';
     Expression: 'Person.allInstances->select(p | p.assets > 200000)';
     Comment: 'An explicit iterator variable, worth naming when the body needs the element twice.'),
    (Category: '3 Filtering';
     Expression: 'Task.allInstances->select(not isCompleted and (priority >= 3))';
     Comment: 'Boolean operators are words: and, or, not. The brackets are load bearing, because and binds tighter than >= here.'),
    (Category: '3 Filtering';
     Expression: 'Person.allInstances->select(lastName = ''Nyman'')';
     Comment: 'A plain equality filter. Bold has no bracket shorthand for select: [] is reserved for qualified associations and is quietly ignored elsewhere.'),
    (Category: '3 Filtering';
     Expression: 'Building.allInstances->select(builtYear < 1990)';
     Comment: 'Filtering a superclass extent on an attribute both subclasses inherit.'),
    (Category: '3 Filtering';
     Expression: 'Project.allInstances->select(manager.isNull)';
     Comment: 'isNull tests an unset single link. Comparing to nil directly would not do.'),

    // -- Collect -----------------------------------------------------------
    (Category: '4 Collect';
     Expression: 'Person.allInstances.fullName';
     Comment: 'Implicit collect: naming an attribute after a collection maps over it.'),
    (Category: '4 Collect';
     Expression: 'Person.allInstances->collect(firstName + '' '' + lastName)';
     Comment: 'Explicit collect, needed as soon as the body is an expression and not a member.'),
    (Category: '4 Collect';
     Expression: 'Project.allInstances->collect(tasks->size)';
     Comment: 'One number per project. The result is a list of Integers, not of objects.'),
    (Category: '4 Collect';
     Expression: 'Building.allInstances->collect(address + '', '' + city)';
     Comment: 'String concatenation inside the collect body.'),
    (Category: '4 Collect';
     Expression: 'Person.allInstances.ownedBuildings->asSet->size';
     Comment: 'collect keeps duplicates. asSet removes them, so a co-owned building counts once.'),

    // -- Aggregates --------------------------------------------------------
    (Category: '5 Aggregates';
     Expression: 'Person.allInstances.assets->sum';
     Comment: 'sum over a Currency attribute.'),
    (Category: '5 Aggregates';
     Expression: 'Project.allInstances.budget->sum';
     Comment: 'Total committed budget across every project.'),
    (Category: '5 Aggregates';
     Expression: 'Person.allInstances.assets->maxValue';
     Comment: 'The collection aggregates are maxValue and minValue. Plain max and min are the two-argument functions max(a, b).'),
    (Category: '5 Aggregates';
     Expression: 'Person.allInstances.assets->average';
     Comment: 'average returns a Float even though the input is Currency.'),
    (Category: '5 Aggregates';
     Expression: 'Project.allInstances->collect(tasks->size)->sum';
     Comment: 'An aggregate of an aggregate: total task count reached through the projects.'),
    (Category: '5 Aggregates';
     Expression: 'Building.allInstances.builtYear->minValue';
     Comment: 'The oldest building year anywhere in the model.'),
    (Category: '5 Aggregates';
     Expression: 'Task.allInstances->select(isCompleted)->size / Task.allInstances->size';
     Comment: 'Division yields a Float. Compare with the safeDiv entry below.'),
    (Category: '5 Aggregates';
     Expression: 'Task.allInstances->select(priority > 9)->size.safeDiv(0)';
     Comment: 'safeDiv swallows the division by zero. The result is unset, not zero.'),

    // -- Ordering ----------------------------------------------------------
    (Category: '6 Ordering';
     Expression: 'Person.allInstances->orderBy(lastName)';
     Comment: 'orderBy sorts on the body expression.'),
    (Category: '6 Ordering';
     Expression: 'Person.allInstances->orderDescending(assets)';
     Comment: 'The wealthiest person ends up first.'),
    (Category: '6 Ordering';
     Expression: 'Person.allInstances->orderDescending(assets)->first';
     Comment: 'first over an ordered collection is the idiomatic top-one query.'),
    (Category: '6 Ordering';
     Expression: 'Person.allInstances->orderBy(lastName)->at(1)';
     Comment: 'Bold OCL indexes from 1, not from 0. at(0) is an error.'),
    (Category: '6 Ordering';
     Expression: 'Task.allInstances->orderDescending(priority)->last';
     Comment: 'last pairs with first, here giving the lowest priority task.'),

    // -- Types and inheritance --------------------------------------------
    (Category: '7 Types';
     Expression: 'Building.allInstances->select(oclIsKindOf(CommercialBuilding))';
     Comment: 'oclIsKindOf admits subclasses. oclIsTypeOf would demand an exact match.'),
    (Category: '7 Types';
     Expression: 'Building.allInstances->select(oclIsKindOf(ResidentialBuilding))->collect(oclAsType(ResidentialBuilding).monthlyRent)->sum';
     Comment: 'Filter, then downcast with oclAsType to reach an attribute the superclass lacks.'),
    (Category: '7 Types';
     Expression: 'Building.allInstances.oclType';
     Comment: 'oclType gives the runtime class of each object, useful over a mixed extent.'),
    (Category: '7 Types';
     Expression: 'Person.allInstances->first.oclType.asString';
     Comment: 'The class name as a plain string.'),
    (Category: '7 Types';
     Expression: 'CommercialBuilding.allInstances.officeSpace->sum';
     Comment: 'Starting from the subclass extent avoids needing the cast at all.'),

    // -- The association class --------------------------------------------
    (Category: '8 Association class';
     Expression: 'Ownership.allInstances';
     Comment: 'Ownership is a class of its own because the link carries attributes.'),
    (Category: '8 Association class';
     Expression: 'Ownership.allInstances->select(sharedValue > 0.5)';
     Comment: 'Filtering on an attribute that belongs to the link, not to either end.'),
    (Category: '8 Association class';
     Expression: 'Ownership.allInstances->select(sharedValue = 1)->collect(ownedBuildings.address)';
     Comment: 'From the link to one of its ends, then on to an attribute there.'),
    (Category: '8 Association class';
     Expression: 'Person.allInstances->select(ownership->size > 1)';
     Comment: 'People holding more than one stake. This navigates to the link, not past it.'),
    (Category: '8 Association class';
     Expression: 'Building.allInstances->collect(ownership.sharedValue->sum)';
     Comment: 'Share per building. It reads 1 where ownership is fully allocated, 0 where nobody owns it.'),
    (Category: '8 Association class';
     Expression: 'Person.allInstances->first.ownedBuildings';
     Comment: 'The shortcut role hops straight across the link class and lands on Building.'),

    // -- Derived attributes -----------------------------------------------
    (Category: '9 Derived';
     Expression: 'Person.allInstances.fullName';
     Comment: 'fullName has no column. The model derives it in OCL as firstName + a space + lastName.'),
    (Category: '9 Derived';
     Expression: 'Task.allInstances.dueDate';
     Comment: 'dueDate derives as project.endDate + 7, so it moves whenever the project moves.'),
    (Category: '9 Derived';
     Expression: 'Task.allInstances->select(project.isNull)';
     Comment: 'The unfiled task. Its derived dueDate is nil rather than an error, because the derivation guards with isNull.'),

    // -- Strings -----------------------------------------------------------
    (Category: '10 Strings';
     Expression: 'Person.allInstances->collect(lastName.toUpper)';
     Comment: 'toUpper and toLower, applied per element.'),
    (Category: '10 Strings';
     Expression: 'Person.allInstances->select(lastName.sqlLike(''N%''))';
     Comment: 'sqlLike is the string test that survives translation into SQL.'),
    (Category: '10 Strings';
     Expression: 'Building.allInstances->collect(address.length)';
     Comment: 'length of a string attribute.'),
    (Category: '10 Strings';
     Expression: 'Person.allInstances->first.lastName.subString(1, 3)';
     Comment: 'subString counts from 1, matching Delphi strings rather than TStringList.'),
    (Category: '10 Strings';
     Expression: 'Building.allInstances->collect(city)->asSet->orderBy(asString)';
     Comment: 'Distinct cities, sorted. asString is the generic string rendering of any element.'),

    // -- Dates -------------------------------------------------------------
    (Category: '11 Dates';
     Expression: 'Person.allInstances->collect(birthDate.year)';
     Comment: 'The date parts are year, month, day, week and dayOfWeek.'),
    (Category: '11 Dates';
     Expression: 'Person.allInstances->select(birthDate.year < 1980)';
     Comment: 'Filtering on a part of a date rather than on the date itself.'),
    (Category: '11 Dates';
     Expression: 'Project.allInstances->collect(endDate - startDate)';
     Comment: 'Dates are numeric underneath, so subtraction gives a count of days.'),
    (Category: '11 Dates';
     Expression: 'Person.allInstances->orderBy(birthDate)->first.fullName';
     Comment: 'The oldest person, found by ordering on the raw date.'),

    // -- Quantifiers -------------------------------------------------------
    (Category: '12 Quantifiers';
     Expression: 'Project.allInstances->exists(tasks->size = 0)';
     Comment: 'exists stops at the first match and answers with a Boolean.'),
    (Category: '12 Quantifiers';
     Expression: 'Project.allInstances->forAll(budget > 0)';
     Comment: 'forAll is the model-wide sanity check, the same shape a real constraint takes.'),
    (Category: '12 Quantifiers';
     Expression: 'Person.allInstances->select(assignedTasks->forAll(isCompleted))';
     Comment: 'A nested quantifier. It is also true for people with no tasks at all.'),
    (Category: '12 Quantifiers';
     Expression: 'Building.allInstances->exists(b | b.owners->isEmpty)';
     Comment: 'Is any building unowned? A shape worth promoting into a model constraint.'),

    // -- Evaluating in the database ---------------------------------------
    (Category: '13 In the database';
     Expression: 'Person.allInstances->select(assets > 100000)';
     Comment: 'Tick Evaluate in PS for this one. It becomes a WHERE clause instead of a scan.'),
    (Category: '13 In the database';
     Expression: 'Building.allInstances->select(city = ''Helsinki'')';
     Comment: 'Also translatable. Watch the log pane to see what Bold sent to the database.'),
    (Category: '13 In the database';
     Expression: 'Person.allInstances->select(fullName.sqlLike(''A%''))';
     Comment: 'Shows why being an object list is not enough. PS is attempted and the translator refuses, because fullName is derived and transient. The workbench catches that, says so in the log, and evaluates in memory instead.'),
    (Category: '13 In the database';
     Expression: 'Person.allInstances->select(assignedTasks->size >= 2)';
     Comment: 'Counting across a link is beyond the translator, so it falls back to memory.')
  );

function OclSampleCount: Integer;
begin
  Result := Length(cOclSamples);
end;

function OclSample(AIndex: Integer): TOclSample;
begin
  Result := cOclSamples[AIndex];
end;

function SampleDataIsEmpty(ASystem: TBoldSystem): Boolean;
begin
  Result := (ASystem = nil) or
    ASystem.EvaluateExpressionAsBoolean('Person.allInstances->isEmpty');
end;

function SampleDataSummary(ASystem: TBoldSystem): string;

  function Count(const AClassName: string): Integer;
  begin
    Result := ASystem.EvaluateExpressionAsInteger(
      AClassName + '.allInstances->size');
  end;

begin
  if ASystem = nil then
    Exit('no system');
  Result := Format('%d people, %d buildings, %d ownerships, %d projects, %d tasks',
    [Count('Person'), Count('Building'), Count('Ownership'),
     Count('Project'), Count('Task')]);
end;

{ TProject.StartDate is generated into the private section of its class, so it
  cannot be assigned from out here even though OCL reads it perfectly well. The
  generic member API is the way round that: it addresses a member by its model
  expression name and hands back the attribute object itself. }
procedure SetDateMember(AObject: TBoldObject; const AMemberName: string;
  AValue: TDateTime);
begin
  (AObject.BoldMemberByExpressionName[AMemberName] as TBADate).AsDate := AValue;
end;

procedure CreateSampleData(ASystem: TBoldSystem);
var
  Alvar, Britt, Carl, Dinah: TPerson;
  Kaisa, Linnea: TResidentialBuilding;
  Harbour, Torni: TCommercialBuilding;
  Roof, Lobby, Survey: TProject;

  function NewPerson(const AFirst, ALast: string; AAssets: Currency;
    ABirth: TDate; AActive: Boolean): TPerson;
  begin
    Result := TPerson.Create(ASystem);
    Result.FirstName := AFirst;
    Result.LastName := ALast;
    Result.Assets := AAssets;
    Result.BirthDate := ABirth;
    Result.IsActive := AActive;
  end;

  { An association class is never constructed directly: TOwnership.Create raises
    "Cannot create instance of association class". Bold owns the link object's
    lifetime, so the way in is to link the two ends as if the class were not
    there, which makes Bold create the link, and then reach it through the
    link-class role to fill in its own attributes. }
  procedure Own(APerson: TPerson; ABuilding: TBuilding; AShare: Double;
    AAcquired: TDate);
  var
    i: Integer;
    Link: TOwnership;
  begin
    APerson.OwnedBuildings.Add(ABuilding);

    // Find the link that was just made rather than assuming it is the last one,
    // so the order of the seeding calls cannot matter.
    Link := nil;
    for i := 0 to APerson.Ownership.Count - 1 do
      if APerson.Ownership[i].OwnedBuildings = ABuilding then
      begin
        Link := APerson.Ownership[i];
        Break;
      end;
    if Link = nil then
      raise Exception.Create('Bold did not create the Ownership link object.');

    Link.SharedValue := AShare;
    Link.AcquiredDate := AAcquired;
  end;

  procedure NewTask(AProject: TProject; const ATitle: string;
    APriority: Integer; ADone: Boolean; AAssignee: TPerson);
  var
    Task: TTask;
  begin
    Task := TTask.Create(ASystem);
    Task.Title := ATitle;
    Task.Priority := APriority;
    Task.IsCompleted := ADone;
    Task.Project := AProject;
    Task.AssignedTo := AAssignee;
  end;

begin
  // People. Assets and birth years are spread out so the aggregate and date
  // samples have something to separate. Erik is deliberately anonymous here:
  // he owns nothing, lives nowhere and holds no task, which is what makes the
  // empty-collection samples interesting.
  Alvar := NewPerson('Alvar', 'Nyman',    350000, EncodeDate(1968, 3, 14), True);
  Britt := NewPerson('Britt', 'Nyman',    120000, EncodeDate(1975, 11, 2), True);
  Carl  := NewPerson('Carl',  'Sundman',   95000, EncodeDate(1982, 6, 30), False);
  Dinah := NewPerson('Dinah', 'Aalto',    410000, EncodeDate(1990, 1, 19), True);
  NewPerson('Erik', 'Virtanen', 8000, EncodeDate(1999, 9, 5), False);

  // Two of each building subclass, so oclIsKindOf has something to sort.
  Kaisa := TResidentialBuilding.Create(ASystem);
  Kaisa.Address := 'Kaisaniemenkatu 3';
  Kaisa.ZipCode := '00100';
  Kaisa.City := 'Helsinki';
  Kaisa.BuiltYear := 1953;
  Kaisa.Apartments := 24;
  Kaisa.MonthlyRent := 1250;

  Linnea := TResidentialBuilding.Create(ASystem);
  Linnea.Address := 'Linnankatu 12';
  Linnea.ZipCode := '20100';
  Linnea.City := 'Turku';
  Linnea.BuiltYear := 1987;
  Linnea.Apartments := 9;
  Linnea.MonthlyRent := 890;

  Harbour := TCommercialBuilding.Create(ASystem);
  Harbour.Address := 'Satamakatu 5';
  Harbour.ZipCode := '00160';
  Harbour.City := 'Helsinki';
  Harbour.BuiltYear := 2004;
  Harbour.OfficeSpace := 3200;
  Harbour.ParkingSpaces := 45;

  Torni := TCommercialBuilding.Create(ASystem);
  Torni.Address := 'Tornikuja 1';
  Torni.ZipCode := '33100';
  Torni.City := 'Tampere';
  Torni.BuiltYear := 1974;
  Torni.OfficeSpace := 850;
  Torni.ParkingSpaces := 0;

  // Residence is a plain association, quite separate from ownership.
  Alvar.Home := Kaisa;
  Britt.Home := Kaisa;
  Carl.Home := Linnea;
  Dinah.Home := Linnea;

  // Ownership carries a share and a date. Kaisaniemenkatu is split between two
  // owners, two more are held outright, and Tornikuja is left unowned so the
  // isEmpty and sum-to-zero samples have a hit.
  Own(Alvar, Kaisa,   0.6, EncodeDate(2011, 4, 1));
  Own(Britt, Kaisa,   0.4, EncodeDate(2014, 9, 15));
  Own(Dinah, Linnea,  1.0, EncodeDate(2019, 2, 28));
  Own(Dinah, Harbour, 1.0, EncodeDate(2021, 7, 7));

  // TProject.CompleteCreate already assigned a generated name and a start date
  // of today, so both are overwritten to keep the demo reproducible.
  Roof := TProject.Create(ASystem);
  Roof.Name := 'Roof renovation';
  Roof.Description := 'Replace the roof on Kaisaniemenkatu 3';
  Roof.Budget := 180000;
  Roof.Manager := Alvar;
  SetDateMember(Roof, 'startDate', EncodeDate(cAnchorYear, 3, 2));
  Roof.EndDate := EncodeDate(cAnchorYear, 8, 29);

  Lobby := TProject.Create(ASystem);
  Lobby.Name := 'Lobby refit';
  Lobby.Description := 'Rework the ground floor at Satamakatu 5';
  Lobby.Budget := 64000;
  Lobby.Manager := Dinah;
  SetDateMember(Lobby, 'startDate', EncodeDate(cAnchorYear, 5, 11));
  Lobby.EndDate := EncodeDate(cAnchorYear, 6, 30);

  // No manager, so the isNull samples find something.
  Survey := TProject.Create(ASystem);
  Survey.Name := 'Parking survey';
  Survey.Description := 'Still looking for an owner';
  Survey.Budget := 12000;
  SetDateMember(Survey, 'startDate', EncodeDate(cAnchorYear, 1, 8));
  Survey.EndDate := EncodeDate(cAnchorYear, 2, 20);

  NewTask(Roof,   'Scaffolding',      5, True,  Alvar);
  NewTask(Roof,   'Strip old felt',   4, True,  Carl);
  NewTask(Roof,   'Lay membrane',     4, False, Carl);
  NewTask(Roof,   'Final inspection', 2, False, Alvar);
  NewTask(Lobby,  'Choose flooring',  3, True,  Dinah);
  NewTask(Lobby,  'Order furniture',  1, False, Dinah);
  NewTask(Survey, 'Count the bays',   1, False, nil);

  // A task belonging to no project at all, so the isNull samples have a hit and
  // the derived dueDate has a nil project to guard against.
  NewTask(nil,    'Unfiled idea',     2, False, nil);
end;

end.
