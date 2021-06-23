package js7.data.workflow

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.value.{BooleanValue, NamedValues, StringValue}
import js7.data.workflow.WorkflowParameters.{MissingOrderArgumentProblem, UnexpectedOrderArgumentProblem, WrongOrderArgumentTypeProblem}
import org.scalatest.freespec.AnyFreeSpec

final class OrderRequirementsTest extends AnyFreeSpec
{
  private val stringParameter = WorkflowParameter("string", StringValue)
  private val orderRequirements = OrderRequirements(Some(WorkflowParameters.checked(Seq(
    stringParameter,
    WorkflowParameter("string-default", StringValue("DEFAULT"))
  )).orThrow))

  "checkArguments" - {
    "No arguments" in {
      assert(orderRequirements.checkArguments(NamedValues.empty) == Left(
        MissingOrderArgumentProblem(stringParameter).toSerialized))
    }

    "Undeclared argument" in {
      assert(orderRequirements.checkArguments(NamedValues("UNEXPECTED" -> BooleanValue.True)) ==
        Left(Problem.Combined(Set(
          UnexpectedOrderArgumentProblem("UNEXPECTED").toSerialized,
          MissingOrderArgumentProblem(stringParameter).toSerialized))))
    }

    "Wrong type" in {
      assert(orderRequirements.checkArguments(NamedValues("string" -> BooleanValue.True)) == Left(
        WrongOrderArgumentTypeProblem(stringParameter, BooleanValue).toSerialized))
    }

    "Valid arguments" in {
      assert(orderRequirements.checkArguments(NamedValues("string" -> StringValue("STRING"))) == Right(()))
    }
  }

  "defaultArgument" in {
    assert(orderRequirements.defaultArgument("UNKNOWN").isEmpty)
    assert(orderRequirements.defaultArgument("string").isEmpty)
    assert(orderRequirements.defaultArgument("string-default") == Some(StringValue("DEFAULT")))
  }

  "defaultArguments" in {
    assert(orderRequirements.defaultArguments == Map(
      "string-default" -> StringValue("DEFAULT")))
  }
}
