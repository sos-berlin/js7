package com.sos.scheduler.engine.taskserver.modules.shell

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.{RichClosersAny, RichClosersAutoCloseable}
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.minicom.idispatch.{IDispatch, Invocable, InvocableIDispatch, PublicMethodsAreInvocable}
import com.sos.scheduler.engine.taskserver.common.StdFiles
import com.sos.scheduler.engine.taskserver.moduleapi.NamedIDispatches._
import com.sos.scheduler.engine.taskserver.moduleapi.Script
import com.sos.scheduler.engine.taskserver.modules.common.CommonArguments
import com.sos.scheduler.engine.taskserver.modules.javamodule.TestJavaModule
import com.sos.scheduler.engine.taskserver.modules.monitor.Monitor
import com.sos.scheduler.engine.taskserver.modules.shell.ShellProcessTaskTest.{Setting, _}
import com.sos.scheduler.engine.taskserver.spoolerapi.{SpoolerLog, SpoolerTask, TypedNamedIDispatches}
import com.sos.scheduler.engine.taskserver.task.TaskArguments
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
final class ShellProcessTaskTest extends FreeSpec with HasCloser with BeforeAndAfterAll {

  private lazy val actorSystem = ActorSystem("ShellProcessTaskTest") withCloser { _.shutdown() }
  private val synchronizedStartProcess = new StandardRichProcessStartSynchronizer()(actorSystem).closeWithCloser

  override protected def afterAll() = {
    onClose { super.afterAll() }
    closer.close()
  }

  "ShellProcessTask exit 0" in {
    runTask("exit-0",
      Setting(preTask = true, preStep = true, exitCode = 0, postStep = identity),
      expectedSpoolerProcessResult = Some(true))
  }

  "ShellProcessTask exit 0 pretask false" in {
    runTask("exit-0-pretask-false",
      Setting(preTask = false, preStep = true, exitCode = 0, postStep = identity),
      expectedStartResult = false)
  }

  "ShellProcessTask exit 7" in {
    runTask("exit-7",
      Setting(preTask = true, preStep = true, exitCode = 7, postStep = identity),
      expectedSpoolerProcessResult = Some(false))
  }

  private def runTask(id: String, setting: Setting, expectedStartResult: Boolean = true, expectedSpoolerProcessResult: Option[Boolean] = None): Unit = {
    val spoolerLog = new TestSpoolerLog
    val (stepResultOption, files) = autoClosing(newShellProcessTask(id, spoolerLog, setting)) { task ⇒
      val taskResult = task.start() await 60.s
      assert(taskResult == expectedStartResult)
      val r = taskResult.option(task.step())
      // Is not called by C++ Scheduler: task.end()
      task.deleteLogFiles()
      (r, task.files)
    }
    (setting.preTask, stepResultOption) match {
      case (true, Some(stepResult: String)) ⇒
        SafeXML.loadString(stepResult) shouldEqual <process.result
          state_text={s"$TestName=$TestValue"}
          spooler_process_result={expectedSpoolerProcessResult.get.toString}
          exit_code={setting.exitCode.toString}/>
        assert(spoolerLog.infoMessages contains s"[stdout] $TestString")
        assert(spoolerLog.infoMessages contains s"[stdout] $TestName=$TestValue")
        assert((spoolerLog.infoMessages filter ExpectedMonitorMessages.toSet) == ExpectedMonitorMessages)
      case (false, None) ⇒
        val expectedMonitorMessages = List(s"A $PreTaskMessage")
        assert(spoolerLog.infoMessages == expectedMonitorMessages)
      case _ ⇒ fail()
    }
    waitForCondition(timeout = 30.s, step = 100.ms) { files forall { !_.exists }}  // Waiting until the Future in RichProcess.startShellScript has deleted the files
    files filter { _.exists } match {
      case Nil ⇒
      case undeletedFiles ⇒ fail("Files not deleted:\n" + undeletedFiles.mkString("\n"))
    }
  }

  private def newShellProcessTask(id: String, spoolerLog: SpoolerLog, setting: Setting)(implicit ec: ExecutionContext) = {
    val factory = new ShellModule.Factory(synchronizedStartProcess)
    new ShellProcessTask(
      factory.newModule(ShellModule.Arguments(factory, testScript(setting.exitCode))),
      CommonArguments(
        AgentTaskId("1-1"),
        jobName = "TEST-JOB",
        namedIDispatches = TypedNamedIDispatches(List(
          SpoolerLogName → spoolerLog,
          SpoolerTaskName → StubSpoolerTask,
          SpoolerJobName → new IDispatch.Empty {},
          SpoolerName → new IDispatch.Empty {})),
        monitors = List(
          Monitor(TestJavaModule.arguments { new TestMonitor("A", setting) }, name = "Monitor A"),
          Monitor(TestJavaModule.arguments { new TestMonitor("B", setting) }, name = "Monitor B")),
        hasOrder = false,
        stdFiles = StdFiles(stdFileMap = Map(), stderrLogLevel = SchedulerLogLevel.info, log = (_, lines) ⇒ spoolerLog.info(lines))),
      environment = Map(TestName → TestValue),
      variablePrefix = TaskArguments.DefaultShellVariablePrefix,
      logDirectory = temporaryDirectory,
      logFilenamePart = s"ShellProcessTaskTest-$id",
      killScriptOption = None,
      synchronizedStartProcess)
  }
}

private object ShellProcessTaskTest {
  private val TestName = "TESTENV"
  private val TestValue = "TESTENV-VALUE"
  private val TestString = "TEST-SCRIPT"

  private case class Setting(preTask: Boolean, preStep: Boolean, exitCode: Int, postStep: Boolean ⇒ Boolean)

  private def testScript(exitCode: Int) = Script(
    (if (isWindows) s"@echo off\necho $TestName=%$TestName%" else s"echo $TestName=$$$TestName") +
      "\n" +
      s"echo $TestString\n" +
      s"exit $exitCode")

  private object StubSpoolerTask extends SpoolerTask with IDispatch.Empty with Invocable.Empty {
    def setErrorCodeAndText(code: MessageCode, text: String) = throw new NotImplementedError
    def paramsXml = ""
    def paramsXml_=(o: String) = throw new NotImplementedError
    def orderParamsXml = ""
    def orderParamsXml_=(o: String) = throw new NotImplementedError
  }

  private class TestSpoolerLog extends SpoolerLog with PublicMethodsAreInvocable with InvocableIDispatch {
    val infoMessages = mutable.Buffer[String]()

    def log(level: SchedulerLogLevel, message: String) = level match {
      case SchedulerLogLevel.info ⇒ infoMessages ++= message.lines
      case _ ⇒ fail()
    }
  }

  private val PreTaskMessage = "pre-task"
  private val PreStepMessage = "pre-step"
  private val PostStepMessage = "post-step"
  private val PostTaskMessage = "post-task"
  private val ExpectedMonitorMessages = List(
    s"A $PreTaskMessage", s"B $PreTaskMessage",
    s"A $PreStepMessage", s"B $PreStepMessage",
    s"B $PostStepMessage", s"A $PostStepMessage",
    s"B $PostTaskMessage", s"A $PostTaskMessage")

  private class TestMonitor(name: String, setting: Setting) extends sos.spooler.Monitor_impl {
    override def spooler_task_before: Boolean = {
      spooler_log.info(s"$name $PreTaskMessage")
      setting.preTask
    }

    override def spooler_task_after(): Unit =
      spooler_log.info(s"$name $PostTaskMessage")

    override def spooler_process_before = {
      spooler_log.info(s"$name $PreStepMessage")
      setting.preStep
    }

    override def spooler_process_after(returnCode: Boolean): Boolean = {
      spooler_log.info(s"$name $PostStepMessage")
      setting.postStep(returnCode)
    }
  }
}
