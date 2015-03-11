package com.sos.scheduler.engine.taskserver.task.process

/**
 * @author Joacim Zschimmer
 */
object StdoutStderr {
  val StdoutStderrTypes = List[StdoutStderrType](Stdout, Stderr)
  
  sealed abstract class StdoutStderrType(val name: String) {
    override def toString = name
  }
  case object Stdout extends StdoutStderrType("stdout")
  case object Stderr extends StdoutStderrType("stderr")
}
