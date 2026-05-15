package js7.base.io

import java.io.{FilterOutputStream, OutputStream}
import js7.base.io.OpaquePos

/** An OutputStream that can be read starting at any marked position.
  *
  * @see SeekableInputStream */
abstract class SeekableOutputStream(out: OutputStream) extends FilterOutputStream(out):

  /** Finish a chunk and return its position in the output stream. */
  def markOpaquePos(): OpaquePos
