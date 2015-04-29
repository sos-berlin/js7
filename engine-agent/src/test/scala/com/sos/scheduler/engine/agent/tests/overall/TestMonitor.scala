package com.sos.scheduler.engine.agent.tests.overall
import com.sos.scheduler.engine.agent.tests.overall.AgentIT.SignalName
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.File

final class TestMonitor extends sos.spooler.Monitor_impl {

  override def spooler_process_before() = {
    spooler_log.info("SPOOLER_PROCESS_BEFORE")
    new File(spooler_task.params.value(SignalName)).append("x")
    Thread.sleep(2000)
    true
  }

  override def spooler_process_after(returnCode: Boolean) = {
    spooler_log.info("SPOOLER_PROCESS_AFTER")
    returnCode
  }

  override def spooler_task_before(): Boolean = {
    spooler_log.info("SPOOLER_TASK_BEFORE")
    true
  }

  override def spooler_task_after(): Unit = {
    spooler_log.info("SPOOLER_TASK_AFTER")
  }
}
