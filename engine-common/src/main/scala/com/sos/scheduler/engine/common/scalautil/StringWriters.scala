package com.sos.scheduler.engine.common.scalautil

import java.io.StringWriter

object StringWriters {
  def writingString[String](f: StringWriter => Unit) = {
    val w = new StringWriter
    f(w)
    w.toString
  }
}