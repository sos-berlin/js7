package js7.common.scalautil

import java.io.StringWriter

object StringWriters:
  def writingString(f: StringWriter => Unit) =
    val w = new StringWriter
    f(w)
    w.toString
