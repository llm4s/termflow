package fix

import scalafix.v1._
import scala.meta._

class NoDirectRuntimeConfigAccess extends SyntacticRule("NoDirectRuntimeConfigAccess"):
  override def fix(using doc: SyntacticDocument): Patch =
    if !isFrameworkMainFile(doc) || isAllowedConfigFile(doc) then Patch.empty
    else
      doc.tree.collect {
        case term @ Term.Select(Term.Name("sys"), name @ (Name("env") | Name("props"))) =>
          lint(term.pos, s"sys.${name.value}")
        case term @ Term.Apply.Initial(Term.Select(Term.Name("System"), name @ (Name("getenv") | Name("getProperty"))), _) =>
          lint(term.pos, s"System.${name.value}")
      }.asPatch

  private def isFrameworkMainFile(doc: SyntacticDocument): Boolean =
    doc.input match
      case Input.File(path, _)        => path.toString.contains("/modules/termflow/src/main/scala/")
      case Input.VirtualFile(path, _) => path.contains("/modules/termflow/src/main/scala/")
      case _                          => false

  private def isAllowedConfigFile(doc: SyntacticDocument): Boolean =
    doc.input match
      case Input.File(path, _)        => path.toString.endsWith("TermFlowConfig.scala")
      case Input.VirtualFile(path, _) => path.endsWith("TermFlowConfig.scala")
      case _                          => false

  private def lint(pos: Position, target: String): Patch =
    Patch.lint(NoDirectRuntimeConfigAccessDiagnostic(pos, target))

final case class NoDirectRuntimeConfigAccessDiagnostic(position: Position, target: String) extends Diagnostic:
  override def message: String =
    s"Direct runtime config access '$target' is forbidden in framework code. Load config once in TermFlowConfig and pass it explicitly."
