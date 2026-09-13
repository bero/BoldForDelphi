# Release checklist

Tag scheme: `Year.Month.build`, **unpadded** (e.g. 26.8.1 -> next 26.8.2 or 26.9.0).
Never zero-pad the month (Boss/semver rejects leading zeros).

## Build & release
- [ ] CHANGELOG.md updated (docs/changelog.md includes it, do not edit that file); grep docs for stale
      "Current Version" lines (docs/index.md, ROADMAP.md) and add the release to the ROADMAP.md history table
- [ ] Build the 3 design packages for **Win32** (auto-named since LIBSUFFIX AUTO):
      `dclBold280.bpl` (11.3), `dclBold290.bpl` (12.x), `dclBold370.bpl` (13) -> `packages/Bin`
- [ ] Win64 is not part of the release build: 12.3 and 13 can build it (-> `packages/Bin64`),
      11.3 cannot (no 64-bit IDE, no Win64 designide, the build stops with E2202). Nothing from
      `Bin64` is uploaded - see the GitHub release step
- [ ] Remove any stale old-style BPLs (`dclBold.28.bpl` etc.) from `packages/Bin` and `packages/Bin64`
- [ ] `git tag <Y.M.b>` on the release commit; verify local == remote
      (`git ls-remote --tags origin`) before pushing
- [ ] GitHub release with the three **Win32** BPLs from `packages/Bin`, under their own names:
      `gh release upload <Y.M.b> packages/Bin/dclBold280.bpl packages/Bin/dclBold290.bpl packages/Bin/dclBold370.bpl`
- [ ] Do **not** upload the `packages/Bin64` BPLs. `dclBold` is design-only, so a BPL only registers
      components in an IDE - it is not what a Win64 *application* needs (that is `lib\Win64` DCUs,
      which no release ships on either platform). The only audience left is someone running the
      64-bit IDE on Delphi 12 or 13, and DPM already serves them by building locally. Win32-only
      also keeps the asset names as they have always been: release assets are a flat namespace with
      unique names, so shipping both platforms would force a `-Win32`/`-Win64` rename of all of them

## Package-manager manifests (stamp version = tag)
- [ ] `packaging/boss.json` -> move to repo root when Boss goes live; bump `version`
- [ ] `packaging/BoldForDelphi.dspec.yaml` -> bump `version`; libSuffix values are
      280/290/370 since LIBSUFFIX AUTO (NOT 28/29.3/30)
- [ ] `version.txt` in the repo root -> `bero.boldfordelphi: <Y.M.b>` (TMS Smart Setup shows it as the
      product version; the installable versions themselves are the git tags)
- [ ] `packaging/tmsbuild.yaml` unchanged? If it changed, mirror it to the registry folder
      `bero.boldfordelphi/` in tmssoftware/smartsetup-registry via pull request
- [ ] DPM: from the repo root `dpm pack packaging\BoldForDelphi.dspec.yaml -basepath=. -o=<folder>`
      gives three `.dpkg` (delphi11.0 Win32; delphi12.0 and delphi13.0 Win32+Win64 - the platform
      bitmask in the file name reads `...0001` and `...0011`); attach them to the GitHub release and/or
      `dpm push` to https://delphi.dev (account needed). Verify one with `dpm install` into a scratch project
- [ ] GetIt (when live): resubmit to Embarcadero for `Y.M.0` releases only

## Announce
- [ ] Discord (https://discord.gg/C6frzsn), blog

Skipped channels: Delphinus (2026-08-21, repo is a GitHub fork; revisit if detached).
