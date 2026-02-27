# Run TermFlow Sample Apps Quickly

This guide collects working `sbt` commands for launching the sample apps in `modules/termflow-sample`.

## One-off Runs

```bash
# Sample hub dashboard (launches other demos)
sbt "termflowSample/runMain termflow.run.SampleHubMain"

# Echo app
sbt "termflowSample/runMain termflow.apps.echo.EchoApp"

# Counter (sync)
sbt "termflowSample/runMain termflow.apps.counter.SyncCounter"

# Counter (async + spinner)
sbt "termflowSample/runMain termflow.apps.counter.FutureCounter"

# Clock
sbt "termflowSample/runMain termflow.apps.clock.DigitalClock"

# Task manager
sbt "termflowSample/runMain termflow.apps.task.Task"

# Render stress test (high-frequency updates)
sbt "termflowSample/runMain termflow.apps.stress.RenderStressApp"

# Moving sine wave
sbt "termflowSample/runMain termflow.apps.stress.SineWaveApp"

# Tabs demo (switch tabs with independent state)
sbt "termflowSample/runMain termflow.apps.tabs.TabsDemoApp"
```

## Convenience Shell Snippet

Put this in your shell profile (`~/.zshrc` / `~/.bashrc`) for shorter commands:

```bash
termflow-run() {
  local app="$1"
  case "$app" in
    hub) sbt "termflowSample/runMain termflow.run.SampleHubMain" ;;
    echo) sbt "termflowSample/runMain termflow.apps.echo.EchoApp" ;;
    sync-counter) sbt "termflowSample/runMain termflow.apps.counter.SyncCounter" ;;
    future-counter) sbt "termflowSample/runMain termflow.apps.counter.FutureCounter" ;;
    clock) sbt "termflowSample/runMain termflow.apps.clock.DigitalClock" ;;
    task) sbt "termflowSample/runMain termflow.apps.task.Task" ;;
    stress) sbt "termflowSample/runMain termflow.apps.stress.RenderStressApp" ;;
    sine) sbt "termflowSample/runMain termflow.apps.stress.SineWaveApp" ;;
    tabs) sbt "termflowSample/runMain termflow.apps.tabs.TabsDemoApp" ;;
    *)
      echo "Usage: termflow-run {hub|echo|sync-counter|future-counter|clock|task|stress|sine|tabs}"
      return 1
      ;;
  esac
}
```

Then run:

```bash
termflow-run future-counter
```

## Notes

- These apps run in an interactive TUI; use a normal terminal.
- If terminal state looks odd after interruption, run `reset`.
- `termflow.run.TermFlowMain` now defaults to the sample hub when run without args.
