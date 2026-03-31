package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource}
import fs2.Chunk
import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.Deflater
import js7.base.io.{ChunkedDeflaterOutputStream, ChunkedInflaterInputStream}
import js7.base.log.reader.LogFileIndex
import js7.base.log.reader.LogFileIndex.LogWriter

private[reader] case object DeflaterRecompressor extends Recompressor:

  private val dictionary = "".getBytes(UTF_8) // not tested

  def decompressingInputStream(in: InputStream): InputStream =
    ChunkedInflaterInputStream(in, dictionary = dictionary, bufferSize = 4096/*guess*/)

  def toLogWriter(out: OutputStream): Resource[IO, LogWriter] =
    Resource.fromAutoCloseable:
      IO.blocking:
        val deflater = new Deflater(Deflater.BEST_SPEED)
        deflater.setDictionary(dictionary)
        ChunkedDeflaterOutputStream(out, deflater, bufferSize = 32*1024)
    .flatMap: (out: ChunkedDeflaterOutputStream) =>
      Resource.fromAutoCloseable:
        IO:
          new LogFileIndex.LogWriter with AutoCloseable:
            private val buf = new Array[Byte](512)

            def write(chunk: Chunk[Byte]): Unit =
              out.write(chunk.toArray)

            def markPosition() =
              out.finishChunk()

            def close() =
              out.close()
