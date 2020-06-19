package js7.controller.data.javaapi

import js7.controller.data.javaapi.ControllerScriptConverterTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerScriptConverterTest extends AnyFreeSpec
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
