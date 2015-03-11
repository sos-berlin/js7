package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaJoda._
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
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
import org.scalatest.mock.MockitoSugar.mock
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ShellProcessTaskTest extends FreeSpec {

  "ShellProcessTask" in {
    val spoolerLog = new TestSpoolerLog
    val (result, files) = autoClosing(newShellProcessTask(spoolerLog)) { task ⇒
      task.start() shouldBe true
      val result = task.step()
      task.end()
      (result, task.files)
    }
    SafeXML.loadString(result) shouldEqual <process.result
      state_text={s"$TestName=$TestValue"}
      spooler_process_result="false"
      exit_code={ExitCode.toString}/>
    assert(spoolerLog.infoMessages contains TestString)
    assert(spoolerLog.infoMessages contains s"$TestName=$TestValue")
    assert((spoolerLog.infoMessages filter ExpectedMonitorMessages.toSet) == ExpectedMonitorMessages)
    waitForCondition(timeout = 3.s, step = 10.ms) { files forall { !_.exists }}  // Waiting for Future
    files filter { _.exists } match {
      case Nil ⇒
      case undeletedFiles ⇒ fail(s"Files not deleted:\n" + undeletedFiles.mkString("\n"))
    }
  }
}

private object ShellProcessTaskTest {
  private val ExitCode = 7
  private val TestName = "TESTENV"
  private val TestValue = "TESTENV-VALUE"
  private val TestString = "TEST-SCRIPT"
  private val TestScript = Script(
    (if (isWindows) s"@echo off\necho $TestName=%$TestName%" else s"echo $TestName=$$$TestName") +
      "\n" +
      s"echo $TestString\n" +
      s"exit $ExitCode")

  private def newShellProcessTask(spoolerLog: SpoolerLog) =
    new ShellProcessTask(
      ShellModule(TestScript),
      namedInvocables = NamedInvocables(List(
        SpoolerLogName → spoolerLog,
        SpoolerTaskName → TestSpoolerTask,
        SpoolerJobName → mock[Invocable],
        SpoolerName → mock[Invocable])),
      monitors = List(
        Monitor(JavaModule(() ⇒ new AMonitor), name="Monitor A"),
        Monitor(JavaModule(() ⇒ new BMonitor), name="Monitor B")),
      jobName = "TEST-JOB",
      hasOrder = false,
      environment = Map(TestName → TestValue))

  private object TestSpoolerTask extends SpoolerTask {
    def setErrorCodeAndText(code: String, text: String) = throw new NotImplementedError
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

  private val APreTaskMessage = "1 A pre-task"
  private val BPreTaskMessage = "2 B pre-task"
  private val APreStepMessage = "3 A pre-step"
  private val BPreStepMessage = "4 B pre-step"
  private val BPostStepMessage = "5 B post-step"
  private val APostStepMessage = "6 A post-step"
  private val BPostTaskMessage = "7 B post-task"
  private val APostTaskMessage = "8 A post-task"
  private val ExpectedMonitorMessages = List(APreTaskMessage, BPreTaskMessage, APreStepMessage, BPreStepMessage, BPostStepMessage, APostStepMessage, BPostTaskMessage, APostTaskMessage)

  private class AMonitor extends sos.spooler.Monitor_impl {
    override def spooler_task_before: Boolean = {
      spooler_log.info(APreTaskMessage)
      true
    }

    override def spooler_task_after(): Unit =
      spooler_log.info(APostTaskMessage)

    override def spooler_process_before = {
      spooler_log.info(APreStepMessage)
      true
    }

    override def spooler_process_after(returnCode: Boolean): Boolean = {
      spooler_log.info(APostStepMessage)
      returnCode
    }
  }

  private class BMonitor extends sos.spooler.Monitor_impl {
    override def spooler_task_before: Boolean = {
      spooler_log.info(BPreTaskMessage)
      true
    }

    override def spooler_task_after(): Unit =
      spooler_log.info(BPostTaskMessage)

    override def spooler_process_before = {
      spooler_log.info(BPreStepMessage)
      true
    }

    override def spooler_process_after(returnCode: Boolean): Boolean = {
      spooler_log.info(BPostStepMessage)
      returnCode
    }
  }
}
