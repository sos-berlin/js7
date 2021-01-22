package js7.data.workflow

import js7.base.standards.Js7PathValidatorTest
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowPathTest extends AnyFreeSpec
{
  "Invalid path" in {
    Js7PathValidatorTest.checkInvalid("WorkflowPath", WorkflowPath.checked)
  }

  "Valid paths" in {
    Js7PathValidatorTest.checkValid(WorkflowPath.checked)
  }
}
