package js7.base.io

import java.io.{FilterInputStream, InputStream}

/** An InputStream that can be read starting at any chunk. */
abstract class SeekableInputStream(out: InputStream) extends FilterInputStream(out)
