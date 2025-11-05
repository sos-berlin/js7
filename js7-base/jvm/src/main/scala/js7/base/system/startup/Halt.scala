package js7.base.system.startup

import js7.base.log.Logger
import js7.base.log.log4j.Log4j
import js7.base.system.startup.StartUp.printlnWithClockIgnoringException

/**
  * @author Joacim Zschimmer
  */
object Halt:
  private val logger = Logger[this.type]

  def initialize(): Unit = {}

  def haltJava(msg: String, restart: Boolean, warnOnly: Boolean = false): Nothing =
    haltJava2(
      msg,
      exitCode = if restart then Js7ReturnCodes.HaltAndRestart else Js7ReturnCodes.Halt,
      warnOnly = warnOnly)

  private def haltJava2(msg: String, exitCode: Int, warnOnly: Boolean): Nothing =
    try System.err.println()
    catch case _: Throwable => {}

    printlnWithClockIgnoringException(msg)

    try if warnOnly then logger.warn(msg) else logger.error(msg)
    catch case _: Throwable => {}

    try Log4j.shutdown(fast = true)
    catch case _: Throwable => {}

    sys.runtime.halt(exitCode)
    throw new Error("sys.runtime.halt failed")
