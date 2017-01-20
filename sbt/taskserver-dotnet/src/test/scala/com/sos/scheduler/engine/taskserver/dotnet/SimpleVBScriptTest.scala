package com.sos.scheduler.engine.taskserver.dotnet

import com.sos.scheduler.engine.common.system.Bitness.is32Bit
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.taskserver.dotnet.SimpleDotnetTest.TestErrorMessage
import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleReference

/**
  * @author Joacim Zschimmer
  */
final class SimpleVBScriptTest extends SimpleDotnetTest {

  protected def language = "VBScript"

  if (isWindows && is32Bit) {
    def vbscriptRef(script: String) = DotnetModuleReference.ScriptControl(language = "vbscript", script)

    addScriptErrorTest(vbscriptRef(s"""
      Function spooler_process
        Call Err.Raise(99, "VBScript Test", "$TestErrorMessage")
      End Function"""))

    addStandardTest(vbscriptRef("""
      Function spooler_process
        set orderVariables = spooler_task.order.params
        varValue = orderVariables.value("TEST")
        call orderVariables.set_value("TEST", "TEST-CHANGED")
        call spooler_log.log(0, varValue)
        spooler_process = True
      End Function"""))
  }
}
