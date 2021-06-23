package js7.data.workflow

import js7.base.problem.Problem
import js7.data.value.{NumberValue, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowParameterTest extends AnyFreeSpec
{
  "checked" in {
    assert(WorkflowParameter.checked("TEST", NumberValue, Some(StringValue("STRING"))) ==
      Left(Problem("Parameter 'TEST': type of default value does not match parameter type 'Number'")))
    assert(WorkflowParameter.checked("TEST", StringValue, Some(StringValue("STRING"))) ==
      Right(WorkflowParameter("TEST", StringValue("STRING"))))
    assert(WorkflowParameter.checked("TEST", StringValue) ==
      Right(WorkflowParameter("TEST", StringValue)))
  }
}
