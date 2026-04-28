package fix

import scalafix.v1.*

import scala.meta.*

class NoDirectTerminalIO extends SyntacticRule("NoDirectTerminalIO"):
  override def fix(using doc: SyntacticDocument): Patch =
    if !isFrameworkMainFile(doc) then Patch.empty
    else
      doc.tree.collect {
        case term @ Term.Apply.Initial(Term.Name("print"), _) =>
          lint(term.pos, "print")
        case term @ Term.Apply.Initial(Term.Name("println"), _) =>
          lint(term.pos, "println")
        case term @ Term.Select(Term.Name("Console"), name @ (Name("out") | Name("err"))) =>
          lint(term.pos, s"Console.${name.value}")
        case term @ Term.Select(Term.Name("System"), name @ (Name("out") | Name("err") | Name("in"))) =>
          lint(term.pos, s"System.${name.value}")
      }.asPatch

  private val frameworkModulePrefixes: Seq[String] = Seq(
    "/modules/termflow-terminal/src/main/scala/",
    "/modules/termflow-screen/src/main/scala/",
    "/modules/termflow-app/src/main/scala/",
    "/modules/termflow-widgets/src/main/scala/"
  )

  private def isFrameworkPath(s: String): Boolean =
    frameworkModulePrefixes.exists(s.contains)

  private def isFrameworkMainFile(doc: SyntacticDocument): Boolean =
    doc.input match
      case Input.File(path, _)        => isFrameworkPath(path.toString)
      case Input.VirtualFile(path, _) => isFrameworkPath(path)
      case _                          => false

  private def lint(pos: Position, target: String): Patch =
    Patch.lint(NoDirectTerminalIODiagnostic(pos, target))

final case class NoDirectTerminalIODiagnostic(position: Position, target: String) extends Diagnostic:
  override def message: String =
    s"Direct terminal IO '$target' is forbidden in framework code. Route terminal IO through TerminalBackend."
