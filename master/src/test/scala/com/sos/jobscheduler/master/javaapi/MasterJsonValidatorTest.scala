package com.sos.jobscheduler.master.javaapi

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterJsonValidatorTest extends FreeSpec {

  "Valid Workflow" in {
    MasterJsonValidatorTester.testValidWorkflow()
  }

  "Invalid Workflow" in {
    MasterJsonValidatorTester.testInvalidWorkflow()
  }

  "Invalid JSON" in {
    MasterJsonValidatorTester.testInvalidJson()
  }

  "Valid Instruction" in {
    MasterJsonValidatorTester.testValidInstruction()
  }

  "Invalid Instruction" in {
    MasterJsonValidatorTester.testInvalidInstruction()
  }
}
