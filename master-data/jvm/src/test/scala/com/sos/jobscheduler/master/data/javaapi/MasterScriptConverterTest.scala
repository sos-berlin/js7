package com.sos.jobscheduler.master.data.javaapi

import com.sos.jobscheduler.master.data.javaapi.MasterScriptConverterTester._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterScriptConverterTest extends FreeSpec
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
