package js7.base.io

import java.io.OutputStream

object NullOutputStream extends OutputStream:

  def write(b: Int): Unit =
    ()

  override def write(array: Array[Byte]): Unit =
    ()

  override def write(array: Array[Byte], off: Int, len: Int): Unit =
    ()
