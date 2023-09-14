package js7.common.system.startup

import js7.base.log.{Log4j, Logger}
import js7.common.system.startup.StartUp.printlnWithClockIgnoringException

/**
  * @author Joacim Zschimmer
  */
object Halt
{
  private val logger = Logger[this.type]

  def initialize() = {}

  def haltJava(msg: String, restart: Boolean, warnOnly: Boolean = false): Nothing =
    haltJava2(
      msg,
      exitCode = if (restart) Js7ReturnCodes.HaltAndRestart else Js7ReturnCodes.Halt,
      warnOnly = warnOnly)

  private def haltJava2(msg: String, exitCode: Int = Js7ReturnCodes.Halt, warnOnly: Boolean)
  : Nothing = {
    System.err.println()
    printlnWithClockIgnoringException(msg)
    if (warnOnly) logger.warn(msg) else logger.error(msg)
    Log4j.shutdown(fast = true)
    sys.runtime.halt(exitCode)
    throw new Error("sys.runtime.halt failed")
  }
}
