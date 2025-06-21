package js7.base.circeutils

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import scala.collection.immutable.BitSet
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
object ScalaJsonCodecs:

  implicit val FiniteDurationJsonEncoder: Encoder[FiniteDuration] =
    o => Json.fromBigDecimal(BigDecimal(o.toNanos) / 1000000000)

  implicit val FiniteDurationJsonDecoder: Decoder[FiniteDuration] =
    cursor => cursor.value.asNumber match
      case Some(number) =>
        number.toLong match
          case Some(long) => Right(Duration(long, SECONDS))
          case None =>
            number.toBigDecimal match
              case Some(bigDecimal) => Right(Duration((bigDecimal * 1000000000).toLong, NANOSECONDS))
              case None => Left(DecodingFailure(s"Unrecognized duration: ${cursor.value}", cursor.history))
      case None => Left(DecodingFailure(s"Unrecognized FiniteDuration: ${cursor.value}", cursor.history))


  given BitSetCodec: Codec[BitSet] = Codec.from(BitSetDecoder, BitSetEncoder)

  given BitSetEncoder: Encoder[BitSet] =
    bitSet => Json.fromValues(bitSet.view.map(_.asJson))

  given BitSetDecoder: Decoder[BitSet] =
    _.value.as[Array[Int]].map(_.to(BitSet))
