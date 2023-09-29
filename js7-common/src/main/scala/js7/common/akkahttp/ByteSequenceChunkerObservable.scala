package js7.common.akkahttp

import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import monix.reactive.Observable

object ByteSequenceChunkerObservable
{
  object syntax
  {
    implicit class RichByteSequenceChunkerObservable[X](private val observable: Observable[X])
    extends AnyVal
    {
      def chunk[ByteSeq: ByteSequence](chunkSize: Int)
        (implicit ev: ByteSeq =:= X)
      : Observable[ByteSeq] =
        observable.asInstanceOf[Observable[ByteSeq]]
          .flatMap { byteSeq =>
            if byteSeq.length <= chunkSize then
              Observable.pure(byteSeq)
            else {
              def chunk(i: Int): (ByteSeq, Int) =
                (byteSeq.slice(i * chunkSize, (i + 1) * chunkSize), i + 1)
              Observable
                .fromStateAction(chunk)(0)
                .takeWhile(_.nonEmpty)
            }
          }
    }
  }
}
