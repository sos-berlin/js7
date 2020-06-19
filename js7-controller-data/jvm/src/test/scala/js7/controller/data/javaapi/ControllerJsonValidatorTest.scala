package js7.controller.data.javaapi

import js7.controller.data.javaapi.ControllerJsonValidatorTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerJsonValidatorTest extends AnyFreeSpec {

  "Valid Workflow" in {
    testValidWorkflow()
  }

  "Invalid Workflow" in {
    testInvalidWorkflow()
  }

  "Invalid JSON" in {
    testInvalidJson()
  }

  "Valid Instruction" in {
    testValidInstruction()
  }

  "Invalid Instruction" in {
    testInvalidInstruction()
  }
}
