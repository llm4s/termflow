# Contributing

## Prereqs

- JDK 21+
- sbt

## Common commands

- Format: `sbt scalafmtAll`
- Format check (CI): `sbt scalafmtCheckAll`
- Scalafix check: `sbt "scalafixAll --check"`
- Scalafix rewrite: `sbt scalafixAll`
- Tests: `sbt test`
- Publish locally (for integration testing): `sbt publishLocal`

## Scala 3 Style

- Use `enum` for closed sets of variants.
- Use `given` / `using` for context parameters.
- Use `extension` methods instead of implicit classes.
- Do not add implicit conversions; prefer explicit `Tui` construction (for example, `model.tui`).
