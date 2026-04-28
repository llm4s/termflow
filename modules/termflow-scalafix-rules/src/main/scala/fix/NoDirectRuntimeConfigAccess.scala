package fix

import scalafix.v1.*

import scala.meta.*

class NoDirectRuntimeConfigAccess extends SyntacticRule("NoDirectRuntimeConfigAccess"):
  override def fix(using doc: SyntacticDocument): Patch =
    if !isFrameworkMainFile(doc) || isAllowedConfigFile(doc) then Patch.empty
    else
      doc.tree.collect {
        case term @ Term.Select(Term.Name("sys"), name @ (Name("env") | Name("props"))) =>
          lint(term.pos, s"sys.${name.value}")
        case term @ Term.Apply.Initial(
              Term.Select(Term.Name("System"), name @ (Name("getenv") | Name("getProperty"))),
              _
            ) =>
          lint(term.pos, s"System.${name.value}")
      }.asPatch

  private def isFrameworkMainFile(doc: SyntacticDocument): Boolean =
    doc.input match
      case Input.File(path, _)        => isFrameworkPath(path.toString)
      case Input.VirtualFile(path, _) => isFrameworkPath(path)
      case _                          => false

  private val frameworkModulePrefixes: Seq[String] = Seq(
    "/modules/termflow-terminal/src/main/scala/",
    "/modules/termflow-screen/src/main/scala/",
    "/modules/termflow-app/src/main/scala/",
    "/modules/termflow-widgets/src/main/scala/"
  )

  private def isFrameworkPath(s: String): Boolean =
    frameworkModulePrefixes.exists(s.contains)

  private def isAllowedConfigFile(doc: SyntacticDocument): Boolean =
    val allowed = Set("TermFlowConfig.scala", "TerminalBackend.scala")
    doc.input match
      case Input.File(path, _)        => allowed.exists(path.toString.endsWith)
      case Input.VirtualFile(path, _) => allowed.exists(path.endsWith)
      case _                          => false

  private def lint(pos: Position, target: String): Patch =
    Patch.lint(NoDirectRuntimeConfigAccessDiagnostic(pos, target))

final case class NoDirectRuntimeConfigAccessDiagnostic(position: Position, target: String) extends Diagnostic:
  override def message: String =
    s"Direct runtime config access '$target' is forbidden in framework code. Load config once in TermFlowConfig and pass it explicitly."
