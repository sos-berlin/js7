package js7.base.io

import java.io.Writer

/** Writes nothing.
  */
final class NullWriter extends Writer:

  def close(): Unit = ()

  def write(cbuf: Array[Char], off: Int, len: Int): Unit = ()

  def flush(): Unit = ()
