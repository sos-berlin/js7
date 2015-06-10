package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.minicom.idispatch.{Invocable, PublicMethodsAreInvocable}
import com.sos.scheduler.engine.taskserver.module.NamedInvocables.{SpoolerJobName, SpoolerLogName, SpoolerName, SpoolerTaskName}
import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule
import com.sos.scheduler.engine.taskserver.module.{JavaModuleLanguage, NamedInvocables, Script}
import com.sos.scheduler.engine.taskserver.spoolerapi.{SpoolerLog, SpoolerTask}
import com.sos.scheduler.engine.taskserver.task.ShellProcessTaskTest._
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
    runTask(Setting(preTask = true, preStep = true, exitCode = 0, postStep = identity), expectedSpoolerProcessResult = Some(true))
  }

  "ShellProcessTask exit 0 pretask false" in {
    runTask(Setting(preTask = false, preStep = true, exitCode = 0, postStep = identity), expectedStartResult = false)
  }

  "ShellProcessTask exit 7" in {
    runTask(Setting(preTask = true, preStep = true, exitCode = 7, postStep = identity), expectedSpoolerProcessResult = Some(false))
  }

  private def runTask(setting: Setting, expectedStartResult: Boolean = true, expectedSpoolerProcessResult: Option[Boolean] = None): Unit = {
    val spoolerLog = new TestSpoolerLog
    val (stepResultOption, files) = autoClosing(newShellProcessTask(spoolerLog, setting)) { task ⇒
      val taskResult = task.start()
      assert(taskResult == expectedStartResult)
      val r = taskResult.option(task.step())
      // Is not called by C++ Scheduler: task.end()
      (r, task.files)
    }
    (setting.preTask, stepResultOption) match {
      case (true, Some(stepResult: String)) ⇒
        SafeXML.loadString(stepResult) shouldEqual <process.result
          state_text={s"$TestName=$TestValue"}
          spooler_process_result={expectedSpoolerProcessResult.get.toString}
          exit_code={setting.exitCode.toString}/>
        assert(spoolerLog.infoMessages contains TestString)
        assert(spoolerLog.infoMessages contains s"$TestName=$TestValue")
        assert((spoolerLog.infoMessages filter ExpectedMonitorMessages.toSet) == ExpectedMonitorMessages)
      case (false, None) ⇒
        val expectedMonitorMessages = List(s"A $PreTaskMessage")
        assert(spoolerLog.infoMessages == expectedMonitorMessages)
      case _ ⇒ fail()
    }
    waitForCondition(timeout = 3.s, step = 100.ms) { files forall { !_.exists }}  // Waiting until the Future in RichProcess.startShellScript has deleted the files
    files filter { _.exists } match {
      case Nil ⇒
      case undeletedFiles ⇒ fail("Files not deleted:\n" + undeletedFiles.mkString("\n"))
    }
  }
}

private object ShellProcessTaskTest {
  private val TestName = "TESTENV"
  private val TestValue = "TESTENV-VALUE"
  private val TestString = "TEST-SCRIPT"

  private case class Setting(preTask: Boolean, preStep: Boolean, exitCode: Int, postStep: Boolean ⇒ Boolean)

  private def newShellProcessTask(spoolerLog: SpoolerLog, setting: Setting) =
    new ShellProcessTask(
      ShellModule(testScript(setting.exitCode)),
      namedInvocables = NamedInvocables(List(
        SpoolerLogName → spoolerLog,
        SpoolerTaskName → TestSpoolerTask,
        SpoolerJobName → DummyInvocable,
        SpoolerName → DummyInvocable)),
      monitors = List(
        Monitor(new TestModule { def newMonitorInstance() = new TestMonitor("A", setting) }, name="Monitor A"),
        Monitor(new TestModule { def newMonitorInstance() = new TestMonitor("B", setting) }, name="Monitor B")),
      jobName = "TEST-JOB",
      hasOrder = false,
      stdFileMap = Map(),
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

  private trait TestModule extends JavaModule {
    def moduleLanguage = JavaModuleLanguage
    def newJobInstance() = throw new NotImplementedError
  }

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
