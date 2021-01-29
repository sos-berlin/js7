package js7.data.item

import io.circe.Decoder.Result
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.circeutils.typed.TypedJsonCodec.TypeFieldName
import js7.base.utils.Collections.implicits.RichTraversable
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichPartialFunction}
import js7.data.item.SimpleItem._

trait SimpleItem
{
  val companion: Companion

  def id: companion.Id
}

object SimpleItem
{
  trait Companion
  {
    type Item <: SimpleItem
    type Id <: SimpleItemId

    val idCompanion: SimpleItemId.Companion[Id]

    implicit def jsonEncoder: Encoder.AsObject[Item]
    implicit def jsonDecoder: Decoder[Item]

    val name = getClass.simpleScalaName
  }

  def jsonCodec(companions: Seq[Companion]): CirceObjectCodec[SimpleItem] = {
    val typeToCompanion = companions.toKeyedMap(_.name)
    new Encoder.AsObject[SimpleItem] with Decoder[SimpleItem] {
      def encodeObject(item: SimpleItem) = {
        (TypeFieldName -> item.companion.name.asJson) +:
          item.companion.jsonEncoder.encodeObject(item.asInstanceOf[item.companion.Item])
      }

      def apply(cursor: HCursor): Result[SimpleItem] =
        for {
          typeName <- cursor.get[String](TypeFieldName)
          companion <- typeToCompanion.checked(typeName).toDecoderResult(cursor.history)
          item <- companion.jsonDecoder.tryDecode(cursor)
        } yield item
    }
  }
}
