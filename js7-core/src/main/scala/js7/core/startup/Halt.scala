package js7.core.startup

import js7.common.log.Log4j
import js7.common.scalautil.Logger
import js7.core.startup.StartUp.printlnWithClock

/**
  * @author Joacim Zschimmer
  */
object Halt
{
  private val logger = Logger(getClass)

  def haltJava(msg: String, restart: Boolean): Nothing =
    haltJava(msg, exitCode = if (restart) 98 else 99)

  def haltJava(msg: String, exitCode: Int = 99): Nothing = {
    System.err.println()
    printlnWithClock(msg)
    logger.error(msg)
    Log4j.shutdown()
    sys.runtime.halt(exitCode)
    throw new Error("sys.runtime.halt failed")
  }
}
