package termflow.apps.task

import termflow.apps.task.Task.Msg._
import termflow.apps.task.Task.RenderMode._
import termflow.apps.task.Task.{ Model, Msg, RenderMode, TaskId, TaskStatus }
import termflow.tui.Tui
import termflow.tui.Tui.*

object UpdateApp {

  def apply(m: Model, msg: Msg): Tui[Model, Msg] =
    msg match {
      case Msg.ConsoleInputKey(_) =>
        // Handled in Task.App; keep here for exhaustivity
        m.tui
      case Msg.ConsoleInputError(e) =>
        m.copy(renderList = AppErrorMsg(s"Console Input Error: ${e.getMessage}")).tui
      case Msg.Add(id) =>
        if (m.tasks.contains(id)) m.copy(renderList = AppErrorMsg(s"$id already exist")).tui
        else {
          val task     = Task.Task(id, TaskStatus.Pending)
          val newTasks = m.tasks + (id -> task)
          m.copy(
            tasks = newTasks,
            filteredList = recomputeFiltered(newTasks, RenderMode.All),
            renderList = RenderMode.All
          ).tui
        }

      case Msg.Remove(id) =>
        if (!m.tasks.contains(id)) m.copy(renderList = AppErrorMsg(s"$id does not exist")).tui
        else {
          val newTasks = m.tasks - id
          m.copy(tasks = newTasks, filteredList = recomputeFiltered(newTasks, m.renderList)).tui
        }

      case Msg.MarkInProgress(id) =>
        updateTaskStatus(m, id, TaskStatus.InProgress).tui
      case Msg.MarkDone(id) =>
        updateTaskStatus(m, id, TaskStatus.Done).tui
      case Msg.MarkCancel(id) =>
        updateTaskStatus(m, id, TaskStatus.Cancelled).tui
      case ListAll =>
        m.copy(filteredList = recomputeFiltered(m.tasks, All), renderList = All).tui
      case ListInProgress =>
        m.copy(filteredList = recomputeFiltered(m.tasks, InProgress), renderList = InProgress).tui
      case ListDone =>
        m.copy(filteredList = recomputeFiltered(m.tasks, Done), renderList = Done).tui
      case ListCancelled =>
        m.copy(filteredList = recomputeFiltered(m.tasks, Cancelled), renderList = Cancelled).tui
      case InvalidCmd(msg) =>
        m.copy(renderList = AppErrorMsg(msg)).tui
    }

  private def updateTaskStatus(m: Model, id: TaskId, status: TaskStatus): Model =
    if (!m.tasks.contains(id)) m.copy(renderList = AppErrorMsg(s"$id does not exist"))
    else {
      val updated  = m.tasks(id).copy(status = status)
      val newTasks = m.tasks + (id -> updated)
      m.copy(
        tasks = newTasks,
        filteredList = recomputeFiltered(newTasks, m.renderList)
      )
    }

  def replaceInList[A](list: List[A])(predicate: A => Boolean, replacement: A => A): List[A] =
    list.map(item => if (predicate(item)) replacement(item) else item)

  private def recomputeFiltered(tasks: Map[TaskId, Task.Task], mode: RenderMode): List[Task.Task] =
    mode match {
      case All        => tasks.values.toList
      case InProgress => tasks.values.toList.filter(_.status == TaskStatus.InProgress)
      case Done       => tasks.values.toList.filter(_.status == TaskStatus.Done)
      case Cancelled  => tasks.values.toList.filter(_.status == TaskStatus.Cancelled)
      case _          => tasks.values.toList
    }
}
