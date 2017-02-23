package com.sos.scheduler.engine.taskserver.task

import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.types.VariantArray
import com.sos.scheduler.engine.taskserver.data.TaskServerMainTerminated
import com.sos.scheduler.engine.taskserver.dotnet.api.{DotnetModuleInstanceFactory, DotnetModuleReference, TaskContext}
import com.sos.scheduler.engine.taskserver.moduleapi.{ModuleFactoryRegister, Script}
import com.sos.scheduler.engine.taskserver.modules.dotnet.DotnetModule
import com.sos.scheduler.engine.taskserver.modules.javamodule.{JavaScriptEngineModule, StandardJavaModule}
import com.sos.scheduler.engine.taskserver.modules.shell.{RichProcessStartSynchronizer, ShellModule}
import java.nio.file.Paths
import javax.inject.Singleton
import org.scalatest.FreeSpec
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class TaskArgumentsTest extends FreeSpec {

  private val scriptXml = <source><source_part linenr="1">PART-A
</source_part><source_part linenr="2">PART-B</source_part></source>
  private val scriptText = "PART-A\nPART-B"
  private val stubDotnetModuleFactory = {
    val instanceFactory = new DotnetModuleInstanceFactory {
      def newInstance[A](clazz: Class[A], taskContext: TaskContext, reference: DotnetModuleReference) = throw new NotImplementedError
      def close() = throw new NotImplementedError
    }
    new DotnetModule.Factory(instanceFactory, classDllDirectory = Some(Paths.get("/TEST-DLLS")))
  }
  private val injector = Guice.createInjector(new AbstractModule {
    def configure() = {}
    @Provides @Singleton
    def moduleFactoryRegister(shellModuleFactory: ShellModule.Factory): ModuleFactoryRegister =
        new ModuleFactoryRegister(List(
          StandardJavaModule,
          JavaScriptEngineModule,
          shellModuleFactory,
          stubDotnetModuleFactory))

    @Provides @Singleton
    def synchronizedStartProcess: RichProcessStartSynchronizer = RichProcessStartSynchronizer.ForTest

    @Provides @Singleton
    def unitFutureOption: Option[Future[TaskServerMainTerminated.type]] = None
  })

  private val moduleFactoryRegister = injector.instance[ModuleFactoryRegister]

  "jobName" in {
    assert(taskArguments("job=JOBNAME").jobName == "JOBNAME")
  }

  "taskId" in {
    assert(taskArguments("task_id=123").taskId == TaskId(123))
  }

  "language=shell" in {
    assert(moduleArguments("language=shell", s"script=$scriptXml") ==
      ShellModule.Arguments(injector.instance[ShellModule.Factory], Script(scriptText)))
  }

  "language=java" in {
    assert(moduleArguments("language=java", "java_class=com.example.Test") ==
      StandardJavaModule.Arguments("com.example.Test"))
  }

  "language=PowerShell" in {
    assert(moduleArguments("language=PowerShell", s"script=$scriptXml") ==
      DotnetModule.Arguments(stubDotnetModuleFactory, DotnetModuleReference.Powershell(scriptText)))
  }

  "language=ScriptControl:VBScript" in {
    assert(moduleArguments("language=ScriptControl:VBScript", s"script=$scriptXml") ==
      DotnetModule.Arguments(stubDotnetModuleFactory, DotnetModuleReference.ScriptControl(language = "vbscript", script = scriptText)))
  }

  "language=dotnet" in {
    assert(moduleArguments("language=dotnet", "dll=test.dll", "dotnet_class=com.example.Test") ==
      DotnetModule.Arguments(stubDotnetModuleFactory, DotnetModuleReference.DotnetClass(Paths.get("/TEST-DLLS/test.dll"), "com.example.Test")))
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
      "monitor.language=dotNet",
      "monitor.dll=test.dll",
      "monitor.dotnet_class=com.example.C",
      "monitor.script=")))
    assert(a.rawMonitorArguments.size == 3)
    assert(a.rawMonitorArguments(0).name == "")
    assert(a.rawMonitorArguments(0).ordering == 1)
    assert(a.rawMonitorArguments(1).name == "")
    assert(a.rawMonitorArguments(1).ordering == 1)
    assert(moduleFactoryRegister.toModuleArguments(a.rawMonitorArguments(1).rawModuleArguments) == DotnetModule.Arguments(
      stubDotnetModuleFactory,
      DotnetModuleReference.DotnetClass(Paths.get("/TEST-DLLS/test.dll"), className = "com.example.C")))
    assert(a.rawMonitorArguments(2).name == "MONITOR-NAME")
    assert(a.rawMonitorArguments(2).ordering == 7)
  }

  "module (shell)" in {
    assert(moduleArguments(
      "language=shell",
      "script=" + <source><source_part linenr="100">PART-A
</source_part><source_part linenr="200">PART-B</source_part></source>) == ShellModule.Arguments(injector.instance[ShellModule.Factory], Script("PART-A\nPART-B")))
  }

  "module (Java)" in {
    assert(moduleArguments("language=java",
          "script=<source/>",
          "java_class=com.example.Job") == new StandardJavaModule.Arguments(className = "com.example.Job"))
  }

  private def moduleArguments(arguments: String*) =
    moduleFactoryRegister.toModuleArguments(taskArguments(arguments: _*).rawModuleArguments)

  private def taskArguments(arguments: String*) = TaskArguments(VariantArray(arguments.toIndexedSeq))
}
