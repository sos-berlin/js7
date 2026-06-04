package js7.service.lz4

import java.io.{InputStream, OutputStream}
import js7.base.io.{SeekableInputStream, SeekableOutputStream, SeekableOutputStreamTest}
import js7.base.test.OurTestSuite
import net.jpountz.lz4.LZ4Factory

final class Lz4OutputStreamTest extends SeekableOutputStreamTest:

  protected def newChunkedOutputStream(out: OutputStream): SeekableOutputStream =
    new Lz4OutputStream(
      out,
      makeCompressor = () => LZ4Factory.fastestJavaInstance.fastCompressor(),
      bufferSize = Lz4InputStream.DefaultBufferSize)

  protected def newChunkedInputStream(in: InputStream): SeekableInputStream =
    new Lz4InputStream(
      in,
      makeDecompressor = () => LZ4Factory.fastestJavaInstance.fastDecompressor(),
      bufferSize = Lz4InputStream.DefaultBufferSize)
