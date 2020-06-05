package js7.data.workflow.instructions

import js7.base.utils.ScalazStyle._
import js7.data.expression.Expression
import js7.data.job.ReturnCode
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}

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
  implicit val jsonEncoder: Encoder.AsObject[Fail] =
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
