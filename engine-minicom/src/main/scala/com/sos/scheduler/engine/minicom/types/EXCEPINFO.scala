package com.sos.scheduler.engine.minicom.types

/**
 * Like Microsoft's COM EXCEPINFO.
 *
 * @author Joacim Zschimmer
 */
final case class EXCEPINFO(source: String, description: String) {
  override def toString = List(source, description) filter { _.nonEmpty } mkString " "
}

object EXCEPINFO {
  def apply(t: Throwable): EXCEPINFO = new EXCEPINFO(source = "", description = t.toString)
}
