package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.expression.Expression
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
sealed case class Fail(
  message: Option[Expression] = None,
  returnCode: Option[ReturnCode] = None,
  uncatchable: Boolean = false,
  sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)
}

object Fail
{
  implicit val jsonEncoder: ObjectEncoder[Fail] =
    o => JsonObject(
      "message" -> o.message.asJson,
      "returnCode" -> o.returnCode.asJson,
      "uncatchable" -> (o.uncatchable ? o.uncatchable).asJson,
      "sourcePos" -> o.sourcePos.asJson)

  implicit val jsonDecoder: Decoder[Fail] =
    c => for {
      errorMessage <- c.get[Option[Expression]]("message")
      returnCode <- c.get[Option[ReturnCode]]("returnCode")
      uncatchable <- c.get[Option[Boolean]]("uncatchable") map (_ getOrElse false)
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
    } yield Fail(errorMessage, returnCode, uncatchable = uncatchable, sourcePos)
}
