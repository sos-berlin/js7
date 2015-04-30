package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaJoda._
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.minicom.idispatch.{Invocable, PublicMethodsAreInvocable}
import com.sos.scheduler.engine.taskserver.module.NamedInvocables.{SpoolerJobName, SpoolerLogName, SpoolerName, SpoolerTaskName}
import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.module.{NamedInvocables, Script}
import com.sos.scheduler.engine.taskserver.spoolerapi.{SpoolerLog, SpoolerTask}
import com.sos.scheduler.engine.taskserver.task.ShellProcessTaskTest._
import com.sos.scheduler.engine.test.util.time.WaitForCondition.waitForCondition
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ShellProcessTaskTest extends FreeSpec {

  "ShellProcessTask exit 0" in {
    runTask(Setting(exitCode = 0, preTask = true, preStep = true, postStep = identity), expectedSpoolerProcessResult = Some(true))
  }

  "ShellProcessTask exit 0 pretask false" in {
    runTask(Setting(exitCode = 0, preTask = false, preStep = true, postStep = identity), expectedStartResult = false)
  }

  "ShellProcessTask exit 7" in {
    runTask(Setting(exitCode = 7, preTask = true, preStep = true, postStep = identity), expectedSpoolerProcessResult = Some(false))
  }

  private def runTask(setting: Setting, expectedStartResult: Boolean = true, expectedSpoolerProcessResult: Option[Boolean] = None): Unit = {
    val spoolerLog = new TestSpoolerLog
    val (stepResultOption, files) = autoClosing(newShellProcessTask(spoolerLog, setting)) { task ⇒
      val taskResult = task.start()
      assert(taskResult == expectedStartResult)
      val r =
        if (taskResult) Some(task.step())
        else None
      task.end()
      (r, task.files)
    }
    stepResultOption match {
      case Some(stepResult: String) ⇒
        SafeXML.loadString(stepResult) shouldEqual <process.result
          state_text={s"$TestName=$TestValue"}
          spooler_process_result={expectedSpoolerProcessResult.get.toString}
          exit_code={setting.exitCode.toString}/>
        assert(spoolerLog.infoMessages contains TestString)
        assert(spoolerLog.infoMessages contains s"$TestName=$TestValue")
        val expectedMonitorMessages = List(
          s"A $PreTaskMessage", s"B $PreTaskMessage",
          s"A $PreStepMessage", s"B $PreStepMessage",
          s"B $PostStepMessage", s"A $PostStepMessage",
          s"B $PostTaskMessage", s"A $PostTaskMessage")
        assert((spoolerLog.infoMessages filter expectedMonitorMessages.toSet) == expectedMonitorMessages)
      case None ⇒
        val expectedMonitorMessages = List(
          s"A $PreTaskMessage",
          s"B $PostTaskMessage", s"A $PostTaskMessage")
        assert((spoolerLog.infoMessages filter expectedMonitorMessages.toSet) == expectedMonitorMessages)
    }
    waitForCondition(timeout = 3.s, step = 10.ms) { files forall { !_.exists }}  // Waiting for Future
    files filter { _.exists } match {
      case Nil ⇒
      case undeletedFiles ⇒ fail(s"Files not deleted:\n" + undeletedFiles.mkString("\n"))
    }
  }
}

private object ShellProcessTaskTest {
  private val TestName = "TESTENV"
  private val TestValue = "TESTENV-VALUE"
  private val TestString = "TEST-SCRIPT"

  private case class Setting(exitCode: Int, preTask: Boolean, preStep: Boolean, postStep: Boolean ⇒ Boolean)

  private def newShellProcessTask(spoolerLog: SpoolerLog, setting: Setting) =
    new ShellProcessTask(
      ShellModule(testScript(setting.exitCode)),
      namedInvocables = NamedInvocables(List(
        SpoolerLogName → spoolerLog,
        SpoolerTaskName → TestSpoolerTask,
        SpoolerJobName → DummyInvocable,
        SpoolerName → DummyInvocable)),
      monitors = List(
        Monitor(JavaModule(() ⇒ new TestMonitor("A", setting)), name="Monitor A"),
        Monitor(JavaModule(() ⇒ new TestMonitor("B", setting)), name="Monitor B")),
      jobName = "TEST-JOB",
      hasOrder = false,
      environment = Map(TestName → TestValue))

  private def testScript(exitCode: Int) = Script(
    (if (isWindows) s"@echo off\necho $TestName=%$TestName%" else s"echo $TestName=$$$TestName") +
      "\n" +
      s"echo $TestString\n" +
      s"exit $exitCode")

  private object DummyInvocable extends Invocable

  private object TestSpoolerTask extends SpoolerTask {
    def setErrorCodeAndText(code: MessageCode, text: String) = throw new NotImplementedError
    def paramsXml = ""
    def paramsXml_=(o: String) = throw new NotImplementedError
    def orderParamsXml = ""
    def orderParamsXml_=(o: String) = throw new NotImplementedError
  }

  private class TestSpoolerLog extends SpoolerLog with PublicMethodsAreInvocable {
    val infoMessages = mutable.Buffer[String]()

    def log(level: SchedulerLogLevel, message: String) = level match {
      case SchedulerLogLevel.info ⇒ infoMessages += message
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
