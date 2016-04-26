package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.types.VariantArray
import com.sos.scheduler.engine.taskserver.module.ModuleArguments.{JavaModuleArguments, ShellModuleArguments}
import com.sos.scheduler.engine.taskserver.module.Script
import java.nio.file.Paths
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TaskArgumentsTest extends FreeSpec {

  private val scriptXml = <source><source_part linenr="1">PART-A
</source_part><source_part linenr="2">PART-B</source_part></source>
  private val scriptText = "PART-A\nPART-B"

  "jobName" in {
    assert(taskArguments("job=JOBNAME").jobName == "JOBNAME")
  }

  "taskId" in {
    assert(taskArguments("task_id=123").taskId == TaskId(123))
  }

  "language=shell" in {
    assert(taskArguments("language=shell", s"script=$scriptXml").moduleArguments ==
      ShellModuleArguments(Script(scriptText)))
  }

  "language=java" in {
    assert(taskArguments("language=java", "java_class=com.example.Test").moduleArguments ==
      JavaModuleArguments("com.example.Test"))
  }

  "hasOrder" in {
    assert(taskArguments("has_order=1").hasOrder)
  }

  "environment" in {
    assert(taskArguments("environment=" + <sos.spooler.variable_set><variable name="A" value="a"/></sos.spooler.variable_set>).environment == Map("A" → "a"))
  }

  "stderr_log_level" in {
    assert(taskArguments().stderrLogLevel contains SchedulerLogLevel.info)
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
    assert(a.monitors(0).moduleArguments == JavaModuleArguments("com.example.B"))
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
    assert(a.moduleArguments == ShellModuleArguments(Script("PART-A\nPART-B")))
  }

  "module (Java)" in {
    val a = TaskArguments(VariantArray(Vector(
      "language=java",
      "script=<source/>",
      "java_class=com.example.Job")))
    assert(a.moduleArguments == JavaModuleArguments(className = "com.example.Job"))
  }

  private def taskArguments(arguments: String*) = TaskArguments(VariantArray(arguments.toIndexedSeq))
}
