# Run TermFlow Sample Apps Quickly

Working `sbt` commands for launching the sample apps in `modules/termflow-sample`.

> **Run these in a real interactive terminal.** TermFlow apps are full-screen
> TUIs driven by JLine; if `sbt` is invoked with stdin/stdout piped or
> redirected, JLine falls back to a dumb terminal and the demos won't render
> correctly. If a TUI exits abnormally and leaves your terminal in a weird
> state, run `reset`.

## Short aliases (recommended)

`build.sbt` defines short command aliases for the headline apps. From the
`sbt` prompt or as a one-shot:

```bash
sbt widgetsDemo       # Layout + Theme + Button/ProgressBar/Spinner/StatusBar
sbt formDemo          # TextField + FocusManager + Keymap multi-field form
sbt catalogDemo       # ListView + Select + Table + TextField task catalog
sbt hubDemo           # Sample hub dashboard (launches other demos)
sbt echoDemo          # Echo / chat-style scrollback
sbt counterDemo       # Sync counter (Layout-based)
sbt futureDemo        # Async counter using Cmd.FCmd
sbt clockDemo         # Digital clock driven by Sub.Every
sbt tabsDemo          # Multi-tab dashboard
sbt taskDemo          # Task manager
sbt stressDemo        # High-frequency renders for repaint stress testing
sbt sineDemo          # Animated sine wave
sbt inputDemo         # Prompt / cursor regression repro (#73 / #74)
```

Inside an `sbt` session you can chain them: `sbt> widgetsDemo`.

## Full `runMain` form

If you need to launch an app the aliases don't cover, the explicit form is:

```bash
sbt "termflowSample/runMain <fully.qualified.AppObject>"
```

For example:

```bash
# Diagnostics demo (logging + render metrics)
sbt "termflowSample/runMain termflow.apps.diagnostics.LoggingMetricsDemoApp"

# Clock variant driven by a custom random source
sbt "termflowSample/runMain termflow.apps.clock.DigitalClockWithRandomSource"

# Provider-chat repro reproduction (chat scrollback + render edge cases)
sbt "termflowSample/runMain termflow.apps.chat.ProviderChatRenderReproMain"
```

## What each headline demo shows

| Alias | App | Demonstrates |
|---|---|---|
| `widgetsDemo` | `WidgetsDemoApp` | The full new stack: `Layout.column`/`row`, a `given Theme` with toggleable dark/light, `Button` focus, `Spinner` + `ProgressBar` driven by `Sub.Every`, `StatusBar` at the bottom, `FocusManager` + `Keymap` for input dispatch |
| `formDemo` | `FormDemoApp` | Multi-field form using three `TextField`s plus Submit / Reset buttons; demonstrates Tab focus cycling and Enter routing across input + button elements |
| `catalogDemo` | `CatalogDemoApp` | Task manager combining `TextField` + `Select` dropdown + `ListView` (scrollable, selectable) + `Table` (live summary). Add tasks, remove them, switch priority — exercises every stateful widget at once. |
| `hubDemo` | `SampleHubApp` | Menu launcher; pick a sub-app by name or number |
| `counterDemo` | `SyncCounter` | Minimal Elm-style app; the simplest example to read |
| `futureDemo` | `FutureCounter` | `Cmd.FCmd` for async work, with a spinner while pending |
| `tabsDemo` | `TabsDemoApp` | Multiple tabs with independent state, layout-driven |
| `clockDemo` | `DigitalClock` | `Sub.Every` ticking at 1 Hz |
| `stressDemo` | `RenderStressApp` | High-frequency updates — useful for spotting flicker |
| `sineDemo` | `SineWaveApp` | Animated sine wave; same purpose as `stressDemo` with smoother motion |
| `inputDemo` | `InputLineReproApp` | Pinned reproduction of the prompt/cursor regressions behind #73 and #74 |

## Widgets demo keys

The widgets demo (`sbt widgetsDemo`) is interactive:

| Key | Action |
|---|---|
| `Tab` | cycle button focus (Save ↔ Cancel) |
| `Enter` / `Space` | activate the focused button |
| `t` | toggle dark / light theme |
| `+` / `-` | nudge progress ± 10 % |
| `q` / `Ctrl+C` / `Esc` | quit |

## Form demo keys

The form demo (`sbt formDemo`) is interactive:

| Key | Action |
|---|---|
| `Tab` / `Shift+Tab` | cycle focus forward / backward (Name → Email → Bio → Submit → Reset) |
| `↑` / `↓` | same as `Shift+Tab` / `Tab` — work anywhere, including inside a text field |
| `←` / `→` (on a button) | previous / next focus |
| `←` / `→` (in a text field) | move the **in-field cursor** (does not change focus) |
| `Enter` (in field) | submit the form (capture all field values) |
| `Enter` / `Space` (button) | activate Submit or Reset |
| `Backspace` / `Delete` / `Home` / `End` | standard text editing in the focused field |
| `Ctrl+T` (anywhere) / `t` (on a button) | toggle dark / light theme |
| `q` (on a button) / `Ctrl+C` / `Esc` | quit |

## Catalog demo keys

The catalog demo (`sbt catalogDemo`) is interactive:

| Key | Action |
|---|---|
| `Tab` / `Shift+Tab` | cycle focus (Task field → Priority → Add → Clear → Task list) |
| `Enter` (in Task field) | add a task with the selected priority |
| `Enter` / `Space` (on Priority) | open the dropdown (or, when open, commit the selection and close) |
| `↑` / `↓` (in open Priority or Task list) | navigate items **within** that widget |
| `Enter` (on a list row) | remove the selected task |
| `Enter` / `Space` (on a button) | activate Add / Clear |
| `←` / `→` (on a button) | previous / next focus |
| `Ctrl+T` (anywhere) / `t` (on a button) | toggle dark / light theme |
| `q` (on a button) / `Ctrl+C` / `Esc` | quit |

## Convenience shell snippet (optional)

If you'd rather have shell-level shortcuts, drop this in `~/.zshrc` /
`~/.bashrc`:

```bash
termflow-run() {
  local app="$1"
  case "$app" in
    hub|widgets|form|catalog|echo|counter|future|clock|tabs|task|stress|sine|input)
      sbt "${app}Demo" ;;
    *)
      echo "Usage: termflow-run {hub|widgets|form|catalog|echo|counter|future|clock|tabs|task|stress|sine|input}"
      return 1
      ;;
  esac
}
```

Then run:

```bash
termflow-run widgets
```

## Notes

- These apps run in an interactive TUI; use a normal terminal.
- If terminal state looks odd after interruption, run `reset`.
- `termflow.run.TermFlowMain` defaults to the sample hub when run without args, so `sbt "termflowSample/runMain termflow.run.TermFlowMain"` is equivalent to `sbt hubDemo`.
