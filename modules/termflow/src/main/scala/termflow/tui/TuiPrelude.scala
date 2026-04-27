package termflow.tui

import scala.concurrent.Future

/**
 * Shared type aliases and lightweight syntax sugar used across TermFlow
 * applications.
 *
 * Import the contents of `TuiPrelude.*` at the top of an app file to
 * unlock:
 *   - `Result[A]` as a shorthand for `Either[TermFlowError, A]`
 *   - `AsyncResult[A]` as `Future[Result[A]]` — the async-with-error idiom
 *     TermFlow shares with the `llm4s` core
 *   - `2.x` / `3.y` coordinate syntax
 *   - `"hello".text` / `"hi".text(fg = Color.Green, bold = true)` styled
 *     text constructors
 *   - `PromptLine`, the opaque type wrapping a submitted prompt line
 */
object TuiPrelude:

  /**
   * Opaque wrapper for a completed line submitted from a prompt.
   *
   * `PromptLine` exists to keep `String`-typed raw input separate from
   * domain strings in `update` / `view` call sites. Construct with
   * `PromptLine(value)` and extract with `line.value`.
   */
  opaque type PromptLine = String
  object PromptLine:

    /** Wrap a raw string as a `PromptLine`. */
    def apply(value: String): PromptLine = value

    /** Extension exposing the underlying string value. */
    extension (line: PromptLine) def value: String = line

  /**
   * Convenience alias for the fallible result of parsing user input or
   * running a validation step.
   *
   * Left-valued results surface through the runtime as
   * [[Cmd.TermFlowErrorCmd]] without terminating the app.
   */
  type Result[A] = Either[TermFlowError, A]

  /**
   * Async-with-typed-error: the future you'd return from a network call,
   * a database lookup, or a tool invocation that can fail in domain ways
   * the caller wants to handle.
   *
   * Mirrors the `AsyncResult[+A]` in the `llm4s` core so apps can pass
   * values across the two libraries without an adapter. Wraps a
   * `scala.concurrent.Future[Result[A]]` — `Result` for logical failures
   * the app should react to, `Future` failures for genuinely
   * unrecoverable infrastructure errors that the runtime surfaces via
   * [[Cmd.TermFlowErrorCmd]].
   *
   * Lift one onto the command bus with [[Cmd.asyncResult]].
   */
  type AsyncResult[+A] = Future[Result[A]]

  object AsyncResult:

    import scala.concurrent.ExecutionContext

    /** Wrap an already-known successful value. */
    def success[A](value: A): AsyncResult[A] = Future.successful(Right(value))

    /** Wrap an already-known logical failure. */
    def failure[A](err: TermFlowError): AsyncResult[A] = Future.successful(Left(err))

    /** Lift a `Result[A]` into an `AsyncResult[A]` without scheduling work. */
    def fromResult[A](r: Result[A]): AsyncResult[A] = Future.successful(r)

    /**
     * Lift a `Future[A]` into an `AsyncResult[A]`, mapping infrastructure
     * failures to a [[TermFlowError.Unexpected]]. Use this at the boundary
     * with libraries that hand you a plain `Future`.
     */
    def fromFuture[A](task: Future[A])(using ec: ExecutionContext): AsyncResult[A] =
      task
        .map(Right(_))
        .recover { case t => Left(TermFlowError.Unexpected(t.getMessage, Some(t))) }

  /**
   * 1-based coordinate syntax: write `2.x` / `10.y` instead of
   * `XCoord(2)` / `YCoord(10)` when building nodes in `view`.
   */
  extension (i: Int)
    def x: XCoord = XCoord(i)
    def y: YCoord = YCoord(i)

  /**
   * Fluent helpers for constructing [[Text]] segments from plain strings.
   *
   * {{{
   * TextNode(2.x, 3.y, List(
   *   "Status: ".text,
   *   "ready".text(fg = Color.Green, bold = true)
   * ))
   * }}}
   */
  extension (txt: String)
    def text: Text                                      = Text(txt, Style())
    def text(style: Style): Text                        = Text(txt, style)
    def text(fg: Color): Text                           = Text(txt, Style(fg = fg))
    def text(fg: Color, bg: Color): Text                = Text(txt, Style(fg = fg, bg = bg))
    def text(fg: Color, bg: Color, bold: Boolean): Text = Text(txt, Style(fg = fg, bg = bg, bold = bold))
    def text(fg: Color, bg: Color, bold: Boolean, underline: Boolean): Text =
      Text(txt, Style(fg = fg, bg = bg, bold = bold, underline = underline))
    def text(fg: Color, bg: Color, bold: Boolean, underline: Boolean, border: Boolean): Text =
      Text(txt, Style(fg = fg, bg = bg, bold = bold, underline = underline, border = border))
