package js7.common.system.startup

import js7.base.log.{Log4j, Logger}
import js7.common.system.startup.StartUp.printlnWithClockIgnoringException

/**
  * @author Joacim Zschimmer
  */
object Halt
{
  private val logger = Logger(getClass)

  def initialize() = {}

  def haltJava(msg: String, restart: Boolean): Nothing =
    haltJava(msg, exitCode =
      if (restart) Js7ReturnCodes.HaltAndRestart
      else Js7ReturnCodes.Halt)

  private def haltJava(msg: String, exitCode: Int = Js7ReturnCodes.Halt): Nothing = {
    System.err.println()
    printlnWithClockIgnoringException(msg)
    logger.error(msg)
    Log4j.shutdown()
    sys.runtime.halt(exitCode)
    throw new Error("sys.runtime.halt failed")
  }
}
