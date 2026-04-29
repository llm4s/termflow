# API reference (Scaladoc)

The full API documentation is generated from source via `sbt unidoc` and
deployed alongside this site:

→ **<a href="../api/index.html" target="_blank">Browse the Scaladoc</a>**

The Scaladoc covers the four published modules:

| Module | Coordinates |
|---|---|
| `termflow-terminal` | `org.llm4s::termflow-terminal:0.2.0` |
| `termflow-screen`   | `org.llm4s::termflow-screen:0.2.0`   |
| `termflow-app`      | `org.llm4s::termflow-app:0.2.0`      |
| `termflow-widgets`  | `org.llm4s::termflow-widgets:0.2.0`  |

Plus the umbrella `termflow` artefact that depends on all four, and the
`termflow-testkit` artefact for testing.

> The Scaladoc link above resolves once the docs site is deployed via
> the GitHub Pages workflow. Reading this page locally? Run
> `sbt unidoc` to generate Scaladoc into `target/scala-3.X/unidoc/`.
