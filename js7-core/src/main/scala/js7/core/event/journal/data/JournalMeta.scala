package js7.core.event.journal.data

import cats.implicits.toShow
import com.typesafe.scalalogging.Logger
import io.circe.{Decoder, Json}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.event.{Event, JournalHeader, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}

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

  def decodeJson(json: Json): Checked[Any] =
    if (!json.isObject)
      Right(json)  // JournalSeparator
    else
      jsonDecoder.decodeJson(json) match {
        case Left(t: io.circe.DecodingFailure) =>
          val problem = Problem.pure(s"Unexpected JSON: ${t.show}")
          logger.error(s"$problem: ${json.compactPrint.truncateWithEllipsis(100)}")
          Left(problem)

        case Right(o) =>
          Right(o)
      }
}

object JournalMeta
{
  def apply[S <: JournaledState[S]](companion: JournaledState.Companion[S], fileBase: Path) =
    new JournalMeta(companion.snapshotObjectJsonCodec, companion.keyedEventJsonCodec, fileBase)
}
