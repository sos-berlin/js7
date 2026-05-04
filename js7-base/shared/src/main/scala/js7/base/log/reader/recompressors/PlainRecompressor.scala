package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource}
import fs2.Chunk
import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, OutputStream}
import js7.base.log.reader.LogFileIndex.LogWriter
import js7.base.utils.ScalaUtils.syntax.*

private[reader] case object PlainRecompressor extends Recompressor:

  def findRecompressor(name: String) =
    (name == "plain") ? this

  def decompressingInputStream(in: InputStream) =
    new BufferedInputStream(in, 32*1024/*guess*/)

  def toLogWriter(out: OutputStream): Resource[IO, LogWriter] =
    Resource.fromAutoCloseable:
      IO:
        new LogWriter with AutoCloseable:
          private val _out = new BufferedOutputStream(out)
          private var lastByteCount = 0L

          def write(chunk: Chunk[Byte]): Unit =
            _out.write(chunk.toArray)
            lastByteCount += chunk.size

          def markPosition() =
            lastByteCount

          def close() =
            _out.close()
