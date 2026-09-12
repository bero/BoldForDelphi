# Save and Generate

The Bold model round trip, driven from ordinary application code instead of
from the IDE.

When you press **Save and generate** in the Bold model editor inside Delphi, an
IDE expert validates the model, writes it to disk, regenerates your business
classes and offers to evolve the database. That expert is a closed box. This
demo does the same five steps with public framework classes, one button each,
and logs what every step touched.

That makes it useful for two quite different things: understanding what the IDE
actually does on your behalf, and doing it without the IDE at all, for instance
from a build script or a migration tool.

## Running it

1. Build with the **Debug** configuration.
2. Edit `TestSaveAndGenerate.ini` if you want a database. The last step needs
   one; everything before it works without.
3. Run, and press the buttons top to bottom, or **Save and Generate All** to run
   the whole pipeline in order.

The log pane timestamps every step and names every file path it wrote, so
nothing happens that you cannot see.

## The five steps

### 1. Validate

`TBoldUMLModelValidator` checks the model against the type-name dictionary and
reports the highest severity it found. Only `sError` stops the pipeline; hints
and warnings are logged and the run continues.

This is the same validation the IDE runs, and it is worth running on its own
after a hand edit to the model.

### 2. Save the model to a .bld file

`TBoldUMLBldLink.ExportModel` writes the UML model out as `.bld`, Bold's own
textual model format. The path comes from `TDemoDataModule.GetModelFilePath`, so
all the demos in this repository share one model file.

### 3. Generate the business classes

`TBoldGenerator` turns the model into Delphi source. Two settings are worth
noticing in `GenerateCode`:

| Setting | Effect |
| --- | --- |
| `UseTypedLists` | generates a typed list class per class, so `TPersonList` instead of a bare `TBoldObjectList` |
| `EnsureMethodImplementations` | adds empty bodies for methods the model declares but the code does not implement yet |

`EnsureMethodImplementations` is the one that makes regeneration safe to repeat.
It adds what is missing rather than overwriting what you wrote.

### 4. Update the .dfm

This is the step with no equivalent you can call directly, and the most
interesting one to read.

A Bold model is not only a `.bld` file. It is also stored **inside the form
file** of the data module, as a long `Model = (...)` string property on the
`TBoldModel` component. Both copies have to agree, so after writing the `.bld`
the demo splices it back into `DemoDataModule.dfm`.

`UpdateDfmFromBld` does it as a text transformation: copy the form file up to
the line `Model = (`, skip the old model, write each line of the `.bld` with tabs
converted to form-file indentation and every apostrophe escaped as `'#39'`, then
copy the rest. Crude, and entirely dependent on those exact marker lines, which
is worth knowing before you rely on it.

### 5. Detect schema changes and evolve

The demo creates a `TBoldModelChangeTracker` at startup and captures the model
as a baseline. After generation it captures the model again and asks
`RequiresSqlEvolution` whether anything changed that the database would care
about.

That distinction is the point. Renaming a method or adding a derived attribute
changes the model but not the schema. Adding a persistent attribute does. Only
the second kind opens `TfrmBoldDbEvolutor`, which compares the model against the
live database and generates the SQL to bring it into line.

The step is skipped, with a reason in the log, when persistence is XML, when
there is no database handle, or when the database does not exist yet.

## The model editor

**Model Editor** opens `UMLModelEditor.EnsureFormForBoldModel`, the same model
editor form the IDE hosts, inside this plain VCL application. Edit the model
there, close it, then run the pipeline to see your change propagate through all
five steps.

This is also the shortest demonstration that Bold's design-time tooling is
ordinary runtime code, not an IDE plugin.

## What to be careful about

- The pipeline **writes into `examples/Shared`**, which every demo in this
  repository shares. Regenerating here changes the model and the generated
  classes for all of them.
- Step 4 rewrites a form file in place with no backup.
- The evolution dialog can alter a real database schema. Point the ini file at a
  scratch database before pressing anything.

## Files

| File | What it holds |
| --- | --- |
| `TestSaveAndGenerate.dpr` | creates the shared data module before the form |
| `TestMainForm.pas` | the five steps, one private method each |
| `TestSaveAndGenerate.ini` | persistence type and connection settings |

The model, the generated classes and the data module live in
`examples/Shared`, not here.
