# Copilot Instructions

These instructions apply to AI-generated changes in this repository.

## Read First

- `AGENTS.md`
- `CONTRIBUTING.md`
- `docs/development-guidelines.md`

## Working Rules

- Keep PRs small and focused.
- Use significant indentation by default in Scala 3 code.
- Use optional braces only when readability clearly improves.
- Avoid broad style churn in behavior PRs.
- If style churn is large, submit a separate style-only PR.

## Validation

Before proposing changes, run:

- `sbt --batch ciCheck`

When coverage is relevant to library changes, also run:

- `sbt --batch coverageLib`
