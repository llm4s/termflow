# Development Guidelines

## Scope

These guidelines apply to all contributors: humans and AI assistants.

## Scala 3 Style Policy

- Prefer significant indentation as the default style.
- Use optional braces only when they materially improve readability.
- Keep style edits local in feature/bugfix PRs.
- If style churn would dominate a PR, move style changes to a separate style-only PR.

## PR Scoping Rules

- Keep PRs small and focused.
- Separate behavior changes from mechanical/style-only changes.
- New files should follow current style guidance from the start.
- Existing files should be aligned only in touched sections unless the PR is explicitly style-only.

## Compiler Options

Current compiler options are defined in `build.sbt` and should remain documented here:

- `-deprecation`: warn on deprecated APIs.
- `-feature`: warn on feature imports/usages requiring explicit intent.
- `-unchecked`: enable additional unchecked warnings.
- `-Wvalue-discard`: warn when non-`Unit` values are ignored.
- `-Wunused:all`: surface unused imports/vals/params and dead code.
- `-Wsafe-init`: warn about potentially unsafe initialization order.
- `-source:3.7-migration`: emit migration guidance without hard-failing builds.

## AI Assistant Guidance

Use these files as entry points:

- `AGENTS.md`: repo-level instructions for coding agents.
- `CONTRIBUTING.md`: human workflow and command expectations.
- `docs/development-guidelines.md`: style policy, PR scoping, and compiler option intent.

For GitHub-native AI tools, this repo also provides `.github/copilot-instructions.md`.
