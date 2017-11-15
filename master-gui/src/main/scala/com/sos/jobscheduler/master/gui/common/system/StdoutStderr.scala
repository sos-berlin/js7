package com.sos.jobscheduler.master.gui.common.system

/**
 * @author Joacim Zschimmer
 */
object StdoutStderr {
  val Stdout = new StdoutStderrType("stdout")
  val Stderr = new StdoutStderrType("stderr")
  val StdoutStderrTypes = List(Stdout, Stderr)

  final case class StdoutStderrType private[system](string: String) {
    override def toString = string
  }

  object StdoutStderrType {
    val values = Vector(Stdout, Stderr)
  }
}
