# Invoicing - SQLite through Zeos (Lazarus)

The Invoicing scenario on a SQL database under Free Pascal (#117 step 17):
SQLite through [Zeos](https://sourceforge.net/projects/zeoslib) 8.0 and Bold's
Zeos adapter. It is the Free Pascal twin of `..\Console`, which runs the same
scenario on SQLite through FireDAC in Delphi, and of `..\FPC`, which stores in
an XML file and needs no database library.

`InvoicingZeos` loads `..\Model\Invoicing.bld`, creates `InvoicingZeos.db` next
to the program with Bold's schema, runs the scenario from
`..\Tests\InvoicingScenario.pas`, saves, closes, reopens and verifies the
reloaded objects. One line per check; the exit code is the number of failed
checks.

`InvoicingSystemZeos.pas` is the part to copy into an application: a
`TZConnection`, a `TBoldDatabaseAdapterZeos` on it, and a
`TBoldPersistenceHandleDB` between the adapter and the system handle.

## Building and running it

The project requires the Lazarus packages `Bold` and `BoldZeos`
(`packages\Lazarus`), and `BoldZeos` requires Zeos's own package `zcomponent`.
Zeos is not part of this repository: download zeosdbo-8.0.0-stable and set
`ZEOS` to its root folder.

```sh
export ZEOS=~/components/zeosdbo-8.0.0
./build.sh
```

`build.sh` makes the five Zeos packages known to Lazarus
(`lazbuild --add-package-link`), builds `InvoicingZeos.lpi` and runs it. In the
IDE, open `zcomponent.lpk` from `$ZEOS/packages/lazarus` once instead, then
`InvoicingZeos.lpi`.

At run time Zeos loads the SQLite library (`libsqlite3.so.0` on Linux).
