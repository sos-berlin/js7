package js7.base.stream

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import izumi.reflect.Tag
import js7.base.circeutils.CirceUtils.requireJson

/**
  * @param number use 0 when number is irrelevant.
  */
final case class Numbered[+A](number: Long, value: A):

  override def toString =
    s"#$number $value"

  def map[B](f: A => B): Numbered[B] =
    copy[B](value = f(value))


object Numbered:

  given jsonEncoder[A: Encoder]: Encoder[Numbered[A]] =
    case Numbered(0, v) => v.asJson
    case Numbered(n, v) => Json.arr(n.asJson, v.asJson)

  given jsonDecoder[A: {Decoder, Tag as A}]: Decoder[Numbered[A]] =
    c => c.values match
      case Some(fields: IndexedSeq[Json]) =>
        for
          _ <- requireJson(fields.sizeIs == 2, DecodingFailure(
            s"For Numbered[${A.tag}], a JSON array with exactly two elements is expected",
            c.history))
          number <- fields(0).as[Long]
          a <- fields(1).as[A]
        yield
          Numbered(number, a)

      case _ =>
        c.as[A].map(Numbered(0, _))
