package js7.base.io

import java.io.{InputStream, OutputStream}
import js7.base.test.OurTestSuite

final class DeflaterSeekableOutputStreamTest extends SeekableOutputStreamTest:

  protected def newChunkedOutputStream(out: OutputStream): SeekableOutputStream =
    new DeflaterSeekableOutputStream(out)

  protected def newChunkedInputStream(in: InputStream): SeekableInputStream =
    new InflaterSeekableInputStream(in)