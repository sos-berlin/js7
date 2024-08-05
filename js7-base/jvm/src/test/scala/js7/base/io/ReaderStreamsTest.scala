package js7.base.io

import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import java.io.{InputStream, OutputStream, PipedInputStream, PipedOutputStream, PipedReader, PipedWriter, Reader, Writer}
import js7.base.data.ByteArray
import js7.base.io.ReaderStreams.{inputStreamToByteStream, readerToCharStream}
import js7.base.test.OurAsyncTestSuite
import js7.tester.ScalaTestUtils
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.collection.mutable

final class ReaderStreamsTest extends OurAsyncTestSuite:

  "inputStreamToByteStream" in:
    val buffer = mutable.Buffer[ByteArray]()

    def program(in: InputStream) =
      inputStreamToByteStream(in)
        .chunks
        .foreach(chunk => IO:
          buffer += ByteArray.unsafeWrap(chunk.toArray))
        .compile.drain

    val chunks = List("EINS", "ZWEI", "DREI").map(ByteArray.fromString)

    def test(out: OutputStream) = IO:
      for (chunk, i) <- chunks.zipWithIndex yield
        out.write(chunk.unsafeArray)
        out.flush()
        awaitAndAssert(buffer.size == i + 1 && buffer.last == chunk)
      out.close()
      assert(buffer == chunks)

    (for
      out <- Resource.fromAutoCloseable(IO(new PipedOutputStream))
      in <- Resource.fromAutoCloseable(IO(new PipedInputStream(out)))
    yield (out, in))
      .use: (out, in) =>
        IO.both(program(in), test(out))
          .map(_._2)

  "readerToCharStream" in :
    val buffer = mutable.Buffer[String]()

    def program(reader: Reader) =
      readerToCharStream(reader)
        .chunks
        .foreach(chunk => IO:
          buffer += String(chunk.toArray))
        .compile.drain

    val chunks = List("EINS", "ZWEI", "DREI")

    def test(writer: Writer) = IO:
      for (chunk, i) <- chunks.zipWithIndex yield
        writer.write(chunk)
        writer.flush()
        awaitAndAssert(buffer.size == i + 1 && buffer.last == chunk)
      writer.close()
      assert(buffer == chunks)

    (for
      w <- Resource.fromAutoCloseable(IO(new PipedWriter))
      r <- Resource.fromAutoCloseable(IO(new PipedReader(w)))
    yield (w, r))
      .use: (writer, reader) =>
        IO.both(program(reader), test(writer))
          .map(_._2)
