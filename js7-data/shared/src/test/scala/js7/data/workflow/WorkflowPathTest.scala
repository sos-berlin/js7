package js7.data.workflow

import js7.base.standards.Js7PathValidatorTest
import js7.base.test.OurTestSuite

final class WorkflowPathTest extends OurTestSuite
{
  "Invalid path" in {
    Js7PathValidatorTest.checkInvalid("WorkflowPath", WorkflowPath.checked)
  }

  "Valid paths" in {
    Js7PathValidatorTest.checkValid(WorkflowPath.checked)
  }
}
