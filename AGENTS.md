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

## Canonical Reference
- See `docs/development-guidelines.md` for full style policy, PR scoping rules, and compiler option rationale.

</INSTRUCTIONS>
