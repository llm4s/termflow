package termflow.apps.task

import termflow.apps.task.Task.Msg
import termflow.apps.task.Task.TaskId
import termflow.tui.TermFlowError
import termflow.tui.TuiPrelude

import scala.util.Try

import TuiPrelude.Result

object Add:
  def unapply(input: String): Option[String] =
    if input.startsWith("add ") then Some(input.drop(4).trim)
    else None

object Remove:
  def unapply(input: String): Option[String] =
    if input.startsWith("remove ") then Some(input.drop(7).trim)
    else None

object MarkInProgress:
  def unapply(input: String): Option[String] =
    if input.startsWith("inprogress ") then Some(input.drop(11).trim)
    else None

object MarkDone:
  def unapply(input: String): Option[String] =
    if input.startsWith("done ") then Some(input.drop(5).trim)
    else None

object MarkCancel:
  def unapply(input: String): Option[String] =
    if input.startsWith("cancel ") then Some(input.drop(7).trim)
    else None

object GetMsg:
  def apply(input: String): Result[Msg] =
    Try {
      input.trim match
        case Add(taskId) if taskId.nonEmpty            => Msg.Add(TaskId(taskId))
        case Remove(taskId) if taskId.nonEmpty         => Msg.Remove(TaskId(taskId))
        case MarkInProgress(taskId) if taskId.nonEmpty => Msg.MarkInProgress(TaskId(taskId))
        case MarkDone(taskId) if taskId.nonEmpty       => Msg.MarkDone(TaskId(taskId))
        case MarkCancel(taskId) if taskId.nonEmpty     => Msg.MarkCancel(TaskId(taskId))
        case "all"                                     => Msg.ListAll
        case "inprogress"                              => Msg.ListInProgress
        case "done"                                    => Msg.ListDone
        case "canceled"                                => Msg.ListCancelled
        case "exit"                                    => Msg.Exit
        case _                                         => Msg.InvalidCmd(input)
    }.toEither.left.map(e => TermFlowError.Unexpected(e.getMessage))
