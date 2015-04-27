package com.sos.scheduler.engine.agent.tests.overall

final class ThrowingMonitor extends sos.spooler.Monitor_impl {

  override def spooler_process_before = throw new RuntimeException("MONITOR EXCEPTION")
}
