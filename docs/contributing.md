# Contributing

Contributions of every size are welcome:

- **Report issues**: bugs and feature requests go to [GitHub Issues](https://github.com/bero/BoldForDelphi/issues).
- **Send pull requests**: fork the repository, branch from `develop`, and open the pull request against `develop`.
- **Improve the documentation and the examples.**
- **Report compatibility**: how Bold works with your database, Delphi or Lazarus version.
- **Spread the word**: blog posts, talks, the [Discord](https://discord.gg/C6frzsn) server.

The [Roadmap](roadmap.md) lists what is planned and what is under consideration.

## Code changes

### Test first

- For a **bug fix**, first write a test that fails because of the bug, then fix the bug and see the test pass.
- For a **refactoring**, first make sure a test covers the code, and that it passes before and after the change.

[Unit Testing](development/testing.md) shows how to write a test that runs under both Delphi and Free Pascal.

### Before you send a pull request

- Run the Delphi suite on SQLite, the default engine.
- After a change in `Source/Persistence` or `Source/PMapper`, run the engine matrix (`UnitTest/run_matrix.ps1`).
- Units that are part of the Lazarus package `Bold.lpk` must keep compiling with Free Pascal. Run
  `UnitTest/FPC/build.sh` (Linux, or Git Bash on Windows); GitHub Actions runs it on every push as well.
- After a change in `Source`, run `UnitTest/check_no_vcl.ps1`, which checks that the runtime still builds without
  the VCL.

[UnitTest/README.md](https://github.com/bero/BoldForDelphi/blob/develop/UnitTest/README.md) describes each of these
scripts.

### Conventions

- New units are UTF-8 with a BOM. Text files have CRLF line endings in the working tree (see `.gitattributes`).
- Name the unit or the area first in the commit message, and the GitHub issue last, for example
  `TBoldDataSet: the handle stays while the dataset is read with the controls disabled #128`.
- Add a line for a change that users notice to the `[Unreleased]` section of
  [CHANGELOG.md](https://github.com/bero/BoldForDelphi/blob/develop/CHANGELOG.md).

## Documentation

### Where the documentation lives

| What | Where |
|------|-------|
| Overview, features, platforms | `README.md`. Its marked sections are also included into the site's home page and the installation page, so edit them only in the README |
| The documentation site | `docs/`, built with [MkDocs](https://www.mkdocs.org/) and [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/) |
| Changelog and roadmap | `CHANGELOG.md` and `ROADMAP.md`; the site includes them |
| Class reference | `docs/reference/`, generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`. Do not edit it by hand |
| Examples | a `README.md` in each example folder; `docs/examples/index.md` is the catalogue |
| Tests and their scripts | `UnitTest/README.md` |

Keep each fact in one place. When a second page needs it, link to the first, or include the text with a
[snippet section](https://facelessuser.github.io/pymdown-extensions/extensions/snippets/#snippet-sections) as
`docs/index.md` does with the README.

### Preview the site locally

```bash
pip install mkdocs-material mkdocs-git-revision-date-localized-plugin
python -m mkdocs serve
```

Open **http://127.0.0.1:8000/BoldForDelphi/**; the page reloads when you save a file. Before you commit, build the
site the way the deployment does:

```bash
python -m mkdocs build --strict
```

`--strict` turns every broken link and every page missing from the navigation into an error.

### Add a page

1. Create the Markdown file in the folder of its section, for example `docs/concepts/new-topic.md`.
2. Add it to `nav` in `mkdocs.yml`:

    ```yaml
    nav:
      - Core Concepts:
        - concepts/index.md
        - New Topic: concepts/new-topic.md
    ```

3. Link to it from the section's `index.md`.

### Markdown features

Besides standard Markdown, the site has:

- code blocks with a language: ` ```pascal `, ` ```ini `, ` ```powershell `
- admonitions: `!!! note`, `!!! tip`, `!!! warning`, followed by an indented block
- tabs: `=== "Title"`, followed by an indented block
- [Mermaid](https://mermaid.js.org/) diagrams in ` ```mermaid ` blocks
- images in `docs/images/`, linked relatively: `![Model editor](../images/model-editor.png)`

### Deployment

GitHub Actions (`.github/workflows/docs.yml`) builds the site and publishes it to
[bero.github.io/BoldForDelphi](https://bero.github.io/BoldForDelphi/) when a change to `docs/`, `mkdocs.yml`,
`README.md`, `CHANGELOG.md` or `ROADMAP.md` is pushed to `develop`.

### Style

- Write plain, direct English in the present tense. Say first what a thing is and when to use it, then how.
- Keep a page to one topic, and link to related pages instead of repeating them.
- Give code examples that compile against the current source, in ` ```pascal ` blocks.
- Name the toolchain when something is not true for all of them: VCL only, Delphi only, Windows only.
