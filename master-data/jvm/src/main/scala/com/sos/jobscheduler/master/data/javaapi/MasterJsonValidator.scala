package com.sos.jobscheduler.master.data.javaapi

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.Decoder
import java.util.Optional

/**
  * @author Joacim Zschimmer
  */
final class MasterJsonValidator {

  def checkWorkflowJson(jsonString: String): Optional[Problem] =
    checkJson[Workflow](jsonString)

  def checkInstructionJson(jsonString: String): Optional[Problem] =
    checkJson[Instruction](jsonString)

  private def checkJson[A: Decoder](jsonString: String): Optional[Problem] =
    // catchNonFatal, because TypedJsonCodec throws
    jsonString.parseJsonChecked >>= (o ⇒ Checked.catchNonFatal(o.as[A])) >>= (_.toSimpleChecked) match {
      case Valid(_) ⇒ Optional.empty()
      case Invalid(problem) ⇒ Optional.of(problem)
    }
}
