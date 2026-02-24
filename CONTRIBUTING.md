# Contributing

## Prereqs

- JDK 21+
- sbt

## Common commands

- Format: `sbt scalafmtAll`
- CI-equivalent local check: `sbt --batch ciCheck`
- Scalafix rewrite: `sbt scalafixAll`
- Tests: `sbt test`
- Library coverage report: `sbt --batch coverageLib`
- Publish locally (for integration testing): `sbt publishLocal`

## Scala 3 Style

- Use `enum` for closed sets of variants.
- Use `given` / `using` for context parameters.
- Use `extension` methods instead of implicit classes.
- Do not add implicit conversions; prefer explicit `Tui` construction (for example, `model.tui`).
- Prefer significant indentation by default; use optional braces only when readability clearly improves.
- Keep style churn small in feature/bugfix PRs; move broad style rewrites to separate style-only PRs.

## Canonical Guidance

- See `docs/development-guidelines.md` for style policy, PR scoping, and compiler option rationale.
