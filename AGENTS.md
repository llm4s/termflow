<INSTRUCTIONS>
# TermFlow Repo Guidelines

## Structure
- Library code lives in `modules/termflow`.
- Demo apps live in `modules/termflow-sample` and are not published.

## Style
- Use the repo `.scalafmt.conf` (`sbt scalafmtAll`).
- Prefer small, composable functions; avoid side effects in core render/update logic.
- Prefer significant indentation by default; use optional braces only when readability improves.
- Do not submit broad style churn with behavior changes.
- Keep style-only rewrites in separate PRs.

## PR Scope
- Keep PRs small and focused.
- New files must follow current style policy.
- Existing files should be style-aligned only in touched sections, unless the PR is explicitly style-only.

## Local Gates
- Run `./scripts/check_scala3_style.sh` before opening a PR.
- Run `sbt --batch ciCheck` before opening a PR.
- Run `sbt --batch coverageLib` when touching library logic or tests.

</INSTRUCTIONS>
