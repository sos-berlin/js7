package js7.controller.data.javaapi

import io.circe.Decoder
import java.util.Optional
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
final class ControllerJsonValidator
{
  def checkWorkflowJson(jsonString: String): Optional[Problem] =
    checkJson[Workflow](jsonString)

  def checkInstructionJson(jsonString: String): Optional[Problem] =
    checkJson[Instruction](jsonString)

  private def checkJson[A: Decoder](jsonString: String): Optional[Problem] =
    // catchNonFatal, because TypedJsonCodec throws
    jsonString.parseJsonChecked
      .flatMap(o => Checked.catchNonFatal(o.as[A]))
      .flatMap(_.toChecked) match {
        case Right(_) => Optional.empty()
        case Left(problem) => Optional.of(problem)
      }
}
