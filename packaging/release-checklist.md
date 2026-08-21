# Release checklist

Tag scheme: `Year.Month.build`, **unpadded** (e.g. 26.8.1 -> next 26.8.2 or 26.9.0).
Never zero-pad the month (Boss/semver rejects leading zeros).

## Build & release
- [ ] CHANGELOG + docs/changelog updated; grep docs for stale "Current Version" lines
- [ ] Build the 4 design packages (auto-named since LIBSUFFIX AUTO):
      `dclBold280.bpl` (11.3), `dclBold290.bpl` (12.x - one BPL serves 12.1 CE + 12.3,
      pending load test in 12.1 CE), `dclBold370.bpl` (13)
- [ ] Remove any stale old-style BPLs (`dclBold.28.bpl` etc.) from `packages/Bin`
- [ ] `git tag <Y.M.b>` on the release commit; verify local == remote
      (`git ls-remote --tags origin`) before pushing
- [ ] GitHub release with the BPL assets

## Package-manager manifests (stamp version = tag)
- [ ] `packaging/boss.json` -> move to repo root when Boss goes live; bump `version`
- [ ] `packaging/BoldForDelphi.dspec.yaml` -> bump `version`; libSuffix values are
      280/290/370 since LIBSUFFIX AUTO (NOT 28/29.3/30)
- [ ] `tmsbuild.yaml` version file (convention TBD in Phase 1 trial)
- [ ] DPM (when live): `dpm pack` for compilers 11.0/12.0/13.0; attach `.dpkg`
      files to the GitHub release; push to feed
- [ ] GetIt (when live): resubmit to Embarcadero for `Y.M.0` releases only

## Announce
- [ ] Discord (https://discord.gg/C6frzsn), blog

Skipped channels: Delphinus (2026-08-21, repo is a GitHub fork; revisit if detached).
