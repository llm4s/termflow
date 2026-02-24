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
