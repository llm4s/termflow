package termflow.apps.task

import termflow.apps.task.Task._
import termflow.tui.TuiPrelude._
import termflow.tui._

object RenderApp {

  def apply(m: Model): RootNode = {
    val tasks          = m.filteredList
    val taskCount      = tasks.length
    val boxWidth       = math.max(40, m.terminalWidth - 4)
    val helpLines      = 8
    val boxHeight      = math.max(8, 5 + taskCount + helpLines)
    val commandsStartY = 4 + taskCount

    val title = m.renderList match {
      case RenderMode.All        => "📋 All Tasks"
      case RenderMode.InProgress => "🔄 Tasks In Progress"
      case RenderMode.Done       => "✅ Completed Tasks"
      case RenderMode.Cancelled  => "❌ Cancelled Tasks"
      case RenderMode.Count      => "📊 Task Summary"
      case RenderMode.Add        => "➕ New Task Added"
      case RenderMode.Init       => "🎯 Your Tasks"
      case _                     => "📋 Tasks"
    }

    val welcomeText =
      """✨ Welcome to Task Manager! ✨
        |
        |It looks like you don't have any tasks yet.
        |Start by adding your first task using:
        |
        |  add <task id>
        |
        |Let's get things done! 🚀""".stripMargin

    val mainChildren: List[VNode] =
      List(
        BoxNode(1.x, 1.y, boxWidth, boxHeight, children = Nil, style = Style(border = true, fg = Color.Blue)),
        TextNode(2.x, 2.y, List(Text(title, Style(fg = Color.Magenta, bold = true, underline = true))))
      ) ++ {
        if (m.tasks.isEmpty)
          welcomeText.split(System.lineSeparator()).toList.zipWithIndex.map { case (s, i) =>
            TextNode(2.x, (3 + i).y, List(Text(s, Style(fg = Color.Cyan, bold = true))))
          }
        else
          renderTasks(tasks)
      } ++ List(
        TextNode(2.x, commandsStartY.y, List("──────────────────────────────".text(fg = Color.Cyan))),
        TextNode(2.x, (commandsStartY + 1).y, List("Commands:".text(fg = Color.Yellow))),
        TextNode(2.x, (commandsStartY + 2).y, List("  add <id>         → add task".text)),
        TextNode(2.x, (commandsStartY + 3).y, List("  remove <id>      → remove task".text)),
        TextNode(2.x, (commandsStartY + 4).y, List("  inprogress <id>  → mark in progress".text)),
        TextNode(2.x, (commandsStartY + 5).y, List("  done <id>        → mark done".text)),
        TextNode(2.x, (commandsStartY + 6).y, List("  cancel <id>      → cancel task".text)),
        TextNode(2.x, (commandsStartY + 7).y, List("  all|inprogress|done|canceled".text))
      )

    RootNode(
      width = m.terminalWidth,
      height = boxHeight + 6,
      children = mainChildren,
      input = {
        val prefix         = "[]> "
        val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
        Some(
          InputNode(
            2.x,
            (boxHeight + 4).y,
            renderedPrompt.text,
            Style(),
            cursor = renderedPrompt.cursorIndex
          )
        )
      }
    )
  }

  private def renderTasks(tasks: List[Task]): List[VNode] =
    if (tasks.isEmpty)
      List(
        TextNode(
          2.x,
          3.y,
          List(
            Text(
              "No tasks found in this view",
              Style(fg = Color.Yellow, bold = true)
            )
          )
        )
      )
    else
      tasks.zipWithIndex.flatMap { case (task, index) =>
        val (statusIcon, statusColor) = task.status match {
          case TaskStatus.Pending    => ("⏳", Color.Yellow)
          case TaskStatus.InProgress => ("🔄", Color.Blue)
          case TaskStatus.Done       => ("✅", Color.Green)
          case TaskStatus.Cancelled  => ("❌", Color.Red)
        }

        val rowY   = (3 + index).y
        val number = TextNode(2.x, rowY, List(Text(s"${index + 1}.", Style(fg = Color.Black, bold = true))))
        val status = TextNode(6.x, rowY, List(Text(statusIcon, Style(fg = statusColor, bold = true))))
        val taskId = TextNode(10.x, rowY, List(Text(task.id.value, Style(fg = Color.Black, bold = true))))

        List(number, status, taskId)
      }
}
