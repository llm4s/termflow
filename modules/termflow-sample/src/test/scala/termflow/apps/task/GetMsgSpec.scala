package termflow.apps.task

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.task.Task.Msg

class GetMsgSpec extends AnyFunSuite:

  test("parses add command"):
    val result = GetMsg("add task-1")
    assert(result == Right(Msg.Add(Task.TaskId("task-1"))))

  test("parses remove command"):
    val result = GetMsg("remove task-1")
    assert(result == Right(Msg.Remove(Task.TaskId("task-1"))))

  test("parses inprogress command"):
    val result = GetMsg("inprogress task-1")
    assert(result == Right(Msg.MarkInProgress(Task.TaskId("task-1"))))

  test("parses done command"):
    val result = GetMsg("done task-1")
    assert(result == Right(Msg.MarkDone(Task.TaskId("task-1"))))

  test("parses cancel command"):
    val result = GetMsg("cancel task-1")
    assert(result == Right(Msg.MarkCancel(Task.TaskId("task-1"))))

  test("parses list commands"):
    assert(GetMsg("all") == Right(Msg.ListAll))
    assert(GetMsg("inprogress") == Right(Msg.ListInProgress))
    assert(GetMsg("done") == Right(Msg.ListDone))
    assert(GetMsg("canceled") == Right(Msg.ListCancelled))

  test("trims input before parsing"):
    val result = GetMsg("  add task-1  ")
    assert(result == Right(Msg.Add(Task.TaskId("task-1"))))

  test("returns InvalidCmd for unknown command with original input preserved"):
    val input  = "unknown command"
    val result = GetMsg(input)
    assert(result == Right(Msg.InvalidCmd(input)))

  test("returns InvalidCmd for command with missing id"):
    assert(GetMsg("add") == Right(Msg.InvalidCmd("add")))
    assert(GetMsg("remove ") == Right(Msg.InvalidCmd("remove ")))
