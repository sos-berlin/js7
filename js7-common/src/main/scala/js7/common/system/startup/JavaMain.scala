package js7.common.system.startup

import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{Log4j, Logger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages
import js7.common.system.startup.StartUp.printlnWithClock

object JavaMain
{
  private lazy val logger = Logger[this.type]

  def runMain[R](body: => R): R =
    try {
      Log4j.initialize()
      coupleScribeWithSlf4j()
      ProblemCodeMessages.initialize()
      // Initialize class and object for possible quicker emergency stop
      Halt.initialize()
      val r = body
      Log4j.shutdown()
      r
    } catch { case t: Throwable =>
      logger.error(t.toStringWithCauses, t)
      Log4j.shutdown()
      printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
      exit(1)
    }

  def exit(number: Int): Nothing = {
    sys.runtime.exit(number)
    throw new AssertionError("exit failed")
  }
}
