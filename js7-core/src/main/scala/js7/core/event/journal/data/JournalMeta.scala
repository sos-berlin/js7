package js7.core.event.journal.data

import cats.implicits.toShow
import com.typesafe.scalalogging.Logger
import io.circe.{Decoder, Json}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.event.{Event, JournalHeader, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}

/**
  * @author Joacim Zschimmer
  */
final case class JournalMeta(
  snapshotJsonCodec: TypedJsonCodec[Any],
  implicit val eventJsonCodec: KeyedEventTypedJsonCodec[Event],
  /** Path without extension, like "/directory/test". */
  fileBase: Path)
{
  private val logger = Logger(getClass)
  val name = fileBase.getFileName.toString

  private val jsonDecoder: Decoder[Any] = {
    val stampedEventDecoder = implicitly[Decoder[Stamped[KeyedEvent[Event]]]]
    stampedEventDecoder or snapshotJsonCodec or JournalHeader.jsonCodec.asInstanceOf[Decoder[Any]]
  }

  def decodeJsonOrThrow(json: Json): Any =
    if (!json.isObject)
      json  // JournalSeparator
    else
      jsonDecoder.decodeJson(json) match {
        case Left(t) =>
          val msg = s"Unexpected JSON: ${(t: io.circe.DecodingFailure).show}"
          logger.error(s"$msg: ${json.compactPrint.truncateWithEllipsis(100)}")
          new IllegalArgumentException(msg)
        case Right(o) => o
      }
}
