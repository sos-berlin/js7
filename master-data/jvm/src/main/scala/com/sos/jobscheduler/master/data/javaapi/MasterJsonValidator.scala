package com.sos.jobscheduler.master.data.javaapi

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.Decoder
import java.util.Optional

/**
  * @author Joacim Zschimmer
  */
final class MasterJsonValidator
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
