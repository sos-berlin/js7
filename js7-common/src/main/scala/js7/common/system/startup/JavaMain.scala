package js7.common.system.startup

import js7.base.io.process.ReturnCode
import js7.base.log.{Log4j, Logger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages
import js7.common.system.startup.StartUp.printlnWithClock

object JavaMain
{
  private lazy val logger = Logger[this.type]

  def runMain[R](body: => R): R =
    try {
      ProblemCodeMessages.initialize()
      // Initialize class and object for possible quicker emergency stop
      Halt.initialize()
      body
    } catch { case t: Throwable =>
      logger.error(t.toStringWithCauses, t)
      printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
      exit1()
    }

  def exit1(): Nothing = {
    exitIfNonZero(ReturnCode(1))
    throw new AssertionError("exit failed")
  }

  def exitIfNonZero(returnCode: ReturnCode): Unit = {
    Log4j.shutdown()
    if (returnCode.number != 0) System.exit(returnCode.number)
  }
}
