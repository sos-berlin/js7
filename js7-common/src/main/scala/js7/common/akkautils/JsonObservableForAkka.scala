package js7.common.akkautils

import akka.util.ByteString
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.ByteStrings.syntax._
import monix.reactive.Observable

object JsonObservableForAkka
{
  private val defaultBatchSize = 200

  object syntax {
    implicit final class StreamingJsonForAkkaObservable[A0](private val underlying: Observable[A0])
    {
      def encodeJson[A: Encoder](implicit ev: A =:= A0): Observable[ByteString] =
        encodeJson[A](defaultBatchSize)

      def encodeJson[A: Encoder](batchSize: Int)(implicit ev: A =:= A0): Observable[ByteString] =
        underlying.asInstanceOf[Observable[A]]
          .mapParallelOrderedBatch(batchSize = batchSize)(
            encode[A])
    }

    implicit final class StreamingJsonForByteStringObservable(private val underlying: Observable[ByteString])
    {
      def decodeJsonTo[A: Decoder]: Observable[A] =
        decodeJsonTo(defaultBatchSize)

      def decodeJsonTo[A: Decoder](batchSize: Int): Observable[A] =
        underlying.mapParallelOrderedBatch(batchSize = batchSize)(
          decode[A])
    }
  }

  private def encode[A: Encoder](a: A): ByteString =
    a.asJson.toByteSequence[ByteString]

  private def decode[A: Decoder](byteString: ByteString): A =
    byteString.utf8String.parseJson.flatMap(_.as[A].toChecked).orThrow
}
