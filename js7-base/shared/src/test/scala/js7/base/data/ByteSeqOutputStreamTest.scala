package js7.base.data

import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.test.OurTestSuite

final class ByteSeqOutputStreamTest extends OurTestSuite:

  "byteSeq[ByteArray]" in:
    myTest[ByteArray]()

  "byteSeq[fs2.Chunk]" in:
    myTest[fs2.Chunk[Byte]]()

  private def myTest[ByteSeq: ByteSequence as ByteSeq]() =
    val n = 1000
    val out = new ByteSeqOutputStream(initialSize = 10)
    val byteSeqs: Seq[ByteSeq] =
      (0 until n).map: i =>
        out.write(i.toByte)
        out.byteSeq[ByteSeq]
    (0 until n).foreach: i =>
      assert:
        byteSeqs(i) == ByteSeq.unsafeWrap(Array.iterate[Byte](0, i + 1)(i => (i + 1).toByte))
