package js7.base.stream

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.circeutils.CirceUtils.requireJson

final case class Numbered[+A](number: Long, value: A):

  override def toString =
    s"#$number $value"

  def map[B](f: A => B): Numbered[B] =
    copy[B](value = f(value))


object Numbered:
  implicit def jsonEncoder[A: Encoder]: Encoder.AsArray[Numbered[A]] =
    stamped => Vector(
      stamped.number.asJson,
      stamped.value.asJson)

//  implicit def jsonDecoder[A: Decoder]: Decoder[Numbered[A]] =
//    c => {
//      val c1 = c.downArray
//      for {
//        number <- c1.as[Long]
//        a <- c1.downN(1).as[A]
//      } yield Numbered(number, a)
//    }

  implicit def jsonDecoder[A: Decoder]: Decoder[Numbered[A]] =
    c => c.values match
      case Some(fields: IndexedSeq[Json]) =>
        for
          _ <- requireJson(fields.sizeIs == 2, DecodingFailure(
            "For Numbered, a JSON array with exactly two elements expected",
            c.history))
          number <- fields(0).as[Long]
          a <- fields(1).as[A]
        yield Numbered(number, a)

      case _ =>
        c.as[A].map(Numbered(0, _))
        //Left(DecodingFailure("Numbered (a JSON array) expected", c.history))
