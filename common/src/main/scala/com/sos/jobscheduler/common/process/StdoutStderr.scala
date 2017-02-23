package com.sos.jobscheduler.common.process

import com.sos.jobscheduler.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
object StdoutStderr {
  val Stdout = new StdoutStderrType("stdout")
  val Stderr = new StdoutStderrType("stderr")
  val StdoutStderrTypes = List(Stdout, Stderr)

  final case class StdoutStderrType private[process](string: String) extends IsString {
    override def toString = string
  }

  object StdoutStderrType extends IsString.HasJsonFormat[StdoutStderrType]
}
