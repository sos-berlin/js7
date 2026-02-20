package js7.base.fs2utils

import fs2.Chunk
import java.nio.charset.StandardCharsets.UTF_8
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

  "unsafeWrappedArray" in:
    val array = Array[Byte](1, 2, 3)
    assert(Chunk.array(array).unsafeWrappedArray.get eq array)
    assert((Chunk[Byte](1) ++ Chunk[Byte](2)).unsafeWrappedArray.isEmpty)


  "indexOf Chunk" in:
    val chunk = Chunk.from("0123".getBytes(UTF_8))
    assert:
      chunk.indexOf('2') == 2
    assert:
      ByteSeq.indexOf(chunk, '2') == 2

  "indexOf Chunk.ArraySlice" in:
    val chunk = Chunk.from("01234567".getBytes(UTF_8))
    val slice = chunk.slice(2, 5)
    assert(slice == Chunk.from("234".getBytes(UTF_8)))
    assert(slice.indexOf('1') == -1)
    assert(slice.indexOf('2') == 0)
    assert(slice.indexOf('4') == 2)
    assert(slice.indexOf('5') == -1)

    assert(ByteSeq.indexOf(slice, '1') == -1)
    assert(ByteSeq.indexOf(slice, '2') == 0)
    assert(ByteSeq.indexOf(slice, '4') == 2)
    assert(ByteSeq.indexOf(slice, '5') == -1)

    assert(ByteSeq.indexOf(slice, '4', from = 0, until = 1) == -1)
    assert(ByteSeq.indexOf(slice, '4', from = 0, until = 2) == -1)
    assert(ByteSeq.indexOf(slice, '4', from = 0, until = 3) == 2)
    assert(ByteSeq.indexOf(slice, '4', from = 1, until = 1) == -1)
    assert(ByteSeq.indexOf(slice, '4', from = 1, until = 2) == -1)
    assert(ByteSeq.indexOf(slice, '4', from = 1, until = 3) == 2)
    assert(ByteSeq.indexOf(slice, '4', from = 2, until = 2) == -1)
    assert(ByteSeq.indexOf(slice, '4', from = 2, until = 3) == 2)
    assert(ByteSeq.indexOf(slice, '4', from = 3, until = 3) == -1)
    assert(ByteSeq.indexOf(slice, '5', from = 0, until = 99) == -1)
