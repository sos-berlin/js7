package js7.data.workflow.instructions

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.source.SourcePos
import js7.data.value.NamedValues
import js7.data.value.expression.Expression
import js7.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
sealed case class Fail(
  message: Option[Expression] = None,
  namedValues: NamedValues = NamedValues.empty,
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
      "namedValues" -> (o.namedValues.nonEmpty ? o.namedValues).asJson,
      "uncatchable" -> (o.uncatchable ? o.uncatchable).asJson,
      "sourcePos" -> o.sourcePos.asJson)

  implicit val jsonDecoder: Decoder[Fail] =
    c => for {
      errorMessage <- c.get[Option[Expression]]("message")
      namedValues <- c.get[Option[NamedValues]]("namedValues").map(_ getOrElse Map.empty)
      uncatchable <- c.get[Option[Boolean]]("uncatchable").map(_ getOrElse false)
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
    } yield Fail(errorMessage, namedValues, uncatchable = uncatchable, sourcePos)
}
