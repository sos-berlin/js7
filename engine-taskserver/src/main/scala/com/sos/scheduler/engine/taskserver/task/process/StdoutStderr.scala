package com.sos.scheduler.engine.taskserver.task.process

/**
 * @author Joacim Zschimmer
 */
object StdoutStderr {
  val Stdout = new StdoutStderrType("stdout")
  val Stderr = new StdoutStderrType("stderr")
  val StdoutStderrTypes = List(Stdout, Stderr)
  
  final case class StdoutStderrType private[process](name: String) {
    override def toString = name
  }
}
