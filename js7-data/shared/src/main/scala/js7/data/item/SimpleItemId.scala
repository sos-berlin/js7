package js7.data.item

import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString
import js7.base.utils.Collections.implicits.RichTraversable
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.SimpleItemId._

trait SimpleItemId extends GenericString
{
  protected type Self <: SimpleItemId
  def companion: Companion[Self]
}

object SimpleItemId
{
  trait Companion[A <: SimpleItemId] extends GenericString.NameValidating[A]

  type AnyCompanion = Companion[_ <: SimpleItemId]

  implicit val jsonEncoder: Encoder.AsObject[SimpleItemId] = o => JsonObject(
    "TYPE" -> Json.fromString(o.companion.name),
    "id" -> Json.fromString(o.string))

  def jsonDecoder(companions: Iterable[AnyCompanion]): Decoder[SimpleItemId] = {
    val nameToCompanion = companions.toKeyedMap(_.name).checked _
    c => for {
      typ <- c.get[String]("TYPE")
      path <- c.get[String]("id")
      t <- nameToCompanion(typ).toDecoderResult(c.history)
      itemId <- t.checked(path).toDecoderResult(c.history)
    } yield itemId
  }
}
