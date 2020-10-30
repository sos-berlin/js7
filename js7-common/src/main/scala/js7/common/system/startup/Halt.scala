package js7.common.system.startup

import js7.common.log.Log4j
import js7.common.scalautil.Logger
import js7.common.system.startup.StartUp.printlnWithClockIgnoringException

/**
  * @author Joacim Zschimmer
  */
object Halt
{
  private val logger = Logger(getClass)

  Log4j

  def haltJava(msg: String, restart: Boolean): Nothing =
    haltJava(msg, exitCode = if (restart) 98 else 99)

  def haltJava(msg: String, exitCode: Int = 99): Nothing = {
    System.err.println()
    printlnWithClockIgnoringException(msg)
    logger.error(msg)
    Log4j.shutdown()
    sys.runtime.halt(exitCode)
    throw new Error("sys.runtime.halt failed")
  }
}
