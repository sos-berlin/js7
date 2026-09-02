package js7.base.io

import java.io.{FilterInputStream, InputStream}

/** An InputStream that can be read starting at any chunk.
  *
  * Different from FilterInputStream, the constructor is public. */
open class SeekableInputStream(in: InputStream) extends FilterInputStream(in)
