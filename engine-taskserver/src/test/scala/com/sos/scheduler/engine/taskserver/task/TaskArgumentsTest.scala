package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.types.VariantArray
import com.sos.scheduler.engine.taskserver.module.java.StandardJavaModule
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.module.{ModuleLanguage, Script}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TaskArgumentsTest extends FreeSpec {

  "jobName" in {
    assert(taskArguments("job=JOBNAME").jobName == "JOBNAME")
  }

  "taskId" in {
    assert(taskArguments("task_id=123").taskId == TaskId(123))
  }

  "moduleLanguage" in {
    assert(taskArguments("language=shell").moduleLanguage == ModuleLanguage("shell"))
  }

  "script" in {
    assert(taskArguments("script=" + <source><source_part linenr="1">PART-A
</source_part><source_part linenr="2">PART-B</source_part></source>).script == Script("PART-A\nPART-B"))
  }

  "hasOrder" in {
    assert(taskArguments("has_order=1").hasOrder)
  }

  "environment" in {
    assert(taskArguments("environment=" + <sos.spooler.variable_set><variable name="A" value="a"/></sos.spooler.variable_set>).environment == Map("A" → "a"))
  }

  "javaClassNameOption" in {
    assert(taskArguments("java_class=com.example.Test").javaClassNameOption == Some("com.example.Test"))
  }

  "stderr_log_level" in {
    assert(taskArguments(Nil).stderrLogLevel contains SchedulerLogLevel.info)
    assert(taskArguments("stderr_log_level=2").stderrLogLevel contains SchedulerLogLevel.error)
  }

  "monitors" in {
    val a = TaskArguments(VariantArray(Vector(
      "monitor.language=java",
      "monitor.name=MONITOR-NAME",
      "monitor.java_class=com.example.A",
      "monitor.ordering=7",
      "monitor.script=",
      "monitor.language=java",
      "monitor.java_class=com.example.B",
      "monitor.script=",
      "monitor.language=java",
      "monitor.java_class=com.example.C",
      "monitor.script=")))
    assert(a.monitors.size == 3)
    assert(a.monitors(0).name == "")
    assert(a.monitors(0).ordering == 1)
    assert(a.monitors(1).name == "")
    assert(a.monitors(1).ordering == 1)
    assert(a.monitors(2).name == "MONITOR-NAME")
    assert(a.monitors(2).ordering == 7)
  }

  "module (shell)" in {
    val a = TaskArguments(VariantArray(Vector(
      "language=shell",
      "script=" + <source><source_part linenr="100">PART-A
</source_part><source_part linenr="200">PART-B</source_part></source>)))
    assert(a.module == ShellModule(Script("PART-A\nPART-B")))
  }

  "module (Java)" in {
    val a = TaskArguments(VariantArray(Vector(
      "language=java",
      "script=<source/>",
      "java_class=com.example.Job")))
    assert(a.module == StandardJavaModule("com.example.Job"))
  }

  private def taskArguments(argument: String): TaskArguments = taskArguments(Vector(argument))

  private def taskArguments(arguments: Iterable[String]) = TaskArguments(VariantArray(arguments.toIndexedSeq))
}
