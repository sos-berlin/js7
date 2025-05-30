package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.circeutils.typed.TypedJsonCodec.TypeFieldName
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichBoolean

trait PlanStatus:
  def ordinal: Int


object PlanStatus:

  /** Orders may be added. */
  case object Open extends PlanStatus:
    def ordinal = 0

  /** No externals Orders may be added.
    *
    * Internal orders may still be added via workflow instruction. */
  case object Closed extends PlanStatus:
    def ordinal = 1

  /** Like Closed, but has no Orders. */
  final case class Finished(at: Timestamp) extends PlanStatus:
    def ordinal = Finished.ordinal

  case object Finished:
    def ordinal = 2

  /** Plan is closed and deleted. */
  case object Deleted extends PlanStatus:
    def ordinal = 3


  given Encoder[PlanStatus] =
    case Finished(at) =>
      Json.obj(
        TypeFieldName -> "Finished".asJson,
        "at" -> at.asJson)
    case o => o.toString.asJson

  given Decoder[PlanStatus] = c =>
    c.value.asString match
      case Some("Open") => Right(Open)
      case Some("Closed") => Right(Closed)
      case Some("Deleted") => Right(Deleted)
      case _ =>
        for
          typ <- c.get[String](TypeFieldName)
          _ <- (typ == "Finished").orLeft:
            DecodingFailure("Invalid PlanStatus JSON encoding", c.history)
          at <- c.get[Timestamp]("at")
        yield
          Finished(at)

  given Ordering[PlanStatus] = Ordering.by(_.ordinal)
