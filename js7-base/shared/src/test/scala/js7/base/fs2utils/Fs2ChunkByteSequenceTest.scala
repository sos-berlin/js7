package js7.base.fs2utils

import fs2.Chunk
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteSequence, ByteSequenceTester}
import js7.base.fs2utils.Fs2ChunkByteSequence.*

final class Fs2ChunkByteSequenceTest extends ByteSequenceTester[Chunk[Byte]]:

  private val ByteSeq = summon[ByteSequence[Chunk[Byte]]]

  "unsafeWrap, unsafeArray" in:
    val array = Array[Byte](1, 2, 3)
    assert(Chunk.array(array).unsafeArray eq array)
    assert(ByteSeq.unsafeWrap(array).unsafeArray eq array)

  "toByteSequence[Chunk[Byte]]" in:
    val byteString = Chunk[Byte](1.toByte, 2.toByte)
    assert(byteString.toByteSequence[Chunk[Byte]] eq byteString)

  "toChunk[Byte] eq" in:
    val a = Chunk[Byte](1.toByte, 2.toByte)
    assert(a.toChunk eq a)

  "unsafeWrappedArray" in :
    val array = Array[Byte](1, 2, 3)
    assert(Chunk.array(array).unsafeWrappedArray.get eq array)
    assert((Chunk[Byte](1) ++ Chunk[Byte](2)).unsafeWrappedArray.isEmpty)
