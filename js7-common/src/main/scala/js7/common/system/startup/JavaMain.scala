package js7.common.system.startup

import js7.base.BuildInfo
import js7.base.io.process.ReturnCode
import js7.base.log.{Log4j, Logger}
import js7.base.system.startup.StartUp
import js7.base.system.startup.StartUp.printlnWithClock
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages

object JavaMain:
  private lazy val logger = Logger[this.type]

  def runMain[R](name: String)(body: => R): R =
    try
      Logger.initialize(name)
      ProblemCodeMessages.initialize()
      // Initialize class and object for possible quicker emergency stop
      Halt.initialize()
      body
    catch { case t: Throwable =>
      logger.error(t.toStringWithCauses, t)
      printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
      exit1()
    }

  def exit1(): Nothing =
    exitIfNonZero(ReturnCode(1))
    throw new AssertionError("exit failed")

  def exitIfNonZero(returnCode: ReturnCode): Unit =
    Log4j.shutdown()
    if returnCode.number != 0 then System.exit(returnCode.number)
