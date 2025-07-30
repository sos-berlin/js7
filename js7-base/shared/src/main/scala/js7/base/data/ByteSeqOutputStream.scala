package js7.base.data

import java.io.ByteArrayOutputStream

final class ByteSeqOutputStream(initialSize: Int = 32 /*as in ByteArrayOutputStream*/)
extends ByteArrayOutputStream(initialSize):

  def byteSeq[ByteSeq: ByteSequence]: ByteSeq =
    synchronized:
      ByteSequence[ByteSeq].unsafeWrap(buf, 0, count)
