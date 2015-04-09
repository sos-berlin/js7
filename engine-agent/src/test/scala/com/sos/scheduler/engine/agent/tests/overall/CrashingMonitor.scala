package com.sos.scheduler.engine.agent.tests.overall

import com.sos.scheduler.engine.agent.tests.overall.AgentIT._
import java.io.File
import java.lang.Thread.sleep

final class CrashingMonitor extends sos.spooler.Monitor_impl {

   override def spooler_process_before() = {
     val file = new File(spooler_task.params.value(SignalName))
     while (file.length() == 0) sleep(100)
     System.exit(44)
     throw new Error
   }
 }
