package com.sos.jobscheduler.master.data.javaapi

import com.sos.jobscheduler.master.data.javaapi.MasterScriptConverterTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterScriptConverterTest extends AnyFreeSpec
{
  "Workflow JSON to script" in {
    testWorkflowJsonToScript()
  }

  "Workflow script to JSON" in {
    testWorkflowScriptToJson()
  }

  "Workflow script syntax error" in {
    testWorkflowScriptSyntaxError()
  }
}
