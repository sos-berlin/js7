package js7.data.item

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import js7.base.circeutils.CirceCodec
import js7.base.generic.GenericString
import js7.base.utils.Collections.implicits.RichTraversable
import js7.data.item.SimpleItemId._

trait SimpleItemId extends GenericString
{
  protected type Self <: SimpleItemId
  val companion: Companion[Self]

  final def toTypedString: String =
    s"${companion.name}:$string"
}

object SimpleItemId
{
  trait Companion[A <: SimpleItemId] extends GenericString.NameValidating[A]
  {
    type Item = A
  }

  type AnyCompanion = Companion[_ <: SimpleItemId]

  def jsonCodec(companions: Iterable[AnyCompanion]): CirceCodec[SimpleItemId] = {
    val typeToCompanion = companions.toKeyedMap(_.name)

    new Encoder[SimpleItemId] with Decoder[SimpleItemId]
    {
      def apply(a: SimpleItemId) = Json.fromString(a.toTypedString)

      def apply(cursor: HCursor) =
        for {
          string <- cursor.as[String]
          prefixAndPath <- string indexOf ':' match {
            case i if i > 0 => Right((string take i, string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in SimpleItemId: $string", cursor.history))
          }
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          itemId <- typeToCompanion.get(prefix).map(_.apply(path))
            .toRight(DecodingFailure(s"Unrecognized type prefix in SimpleItemId: $prefix", cursor.history))
        } yield itemId
    }
  }
}
