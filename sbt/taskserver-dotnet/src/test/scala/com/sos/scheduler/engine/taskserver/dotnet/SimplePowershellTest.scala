package com.sos.scheduler.engine.taskserver.dotnet

import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.taskserver.dotnet.SimpleDotnetTest.TestErrorMessage
import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleReference
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SimplePowershellTest extends SimpleDotnetTest {

  protected def language = "PowerShell"

  if (isWindows) {
    addScriptErrorTest(DotnetModuleReference.Powershell(s"""
      function spooler_process() {
        throw "$TestErrorMessage"
      }"""))

    addStandardTest(DotnetModuleReference.Powershell(
       """
    function spooler_process() {
      $orderVariables = $spooler_task.order().params()
      $value = $orderVariables.value("TEST")
      $orderVariables.set_value("TEST", "TEST-CHANGED")
      $spooler_log.log(0, $value)
      return $true
    }"""))
  }
}
