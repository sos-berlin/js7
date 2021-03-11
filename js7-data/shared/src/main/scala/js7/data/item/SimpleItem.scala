package js7.data.item

import io.circe.Decoder.Result
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.Subtype
import js7.base.circeutils.typed.TypedJsonCodec.TypeFieldName
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichPartialFunction}
import js7.data.item.SimpleItem._
import scala.reflect.ClassTag

trait SimpleItem
{
  protected type Self <: SimpleItem

  def withRevision(revision: ItemRevision): Self

  val companion: Companion

  def id: companion.Id

  val itemRevision: ItemRevision
}

object SimpleItem
{
  trait Companion
  {
    type Item <: SimpleItem
    type Id <: SimpleItemId

    val idCompanion: SimpleItemId.Companion[Id]
    val name = getClass.simpleScalaName

    def cls: Class[Item]

    implicit def jsonCodec: CirceObjectCodec[Item]

    final def subtype: Subtype[Item] =
      Subtype(jsonCodec)(ClassTag(cls))

    final def jsonEncoder: Encoder.AsObject[Item] =
      jsonCodec

    final def jsonDecoder: Decoder[Item] =
      jsonCodec
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
