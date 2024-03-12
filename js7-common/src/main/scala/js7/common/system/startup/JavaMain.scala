package js7.common.system.startup

import cats.effect.{ExitCode, IO}
import js7.base.io.process.ReturnCode
import js7.base.log.{Log4j, Logger}
import js7.base.system.startup.{Halt, StartUp}
import js7.base.system.startup.StartUp.printlnWithClock
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages

object JavaMain:

  private lazy val logger = Logger[this.type]

  def runMain(name: String)(body: => IO[ProgramTermination]): IO[ProgramTermination] =
    IO.defer:
      Logger.initialize(name)
      initialize()
      body

  def run(name: String)(body: => IO[ExitCode]): IO[ExitCode] =
    Logger.resource[IO](name).surround:
      IO(initialize())
        .*>(body)
        .recover:
          case t: Throwable =>
            logger.error(t.toStringWithCauses, t)
            printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
            ExitCode(1)

  private def initialize(): Unit =
    ProblemCodeMessages.initialize()
    // Initialize class and object for possible quicker emergency stop
    Halt.initialize()

  @deprecated("Use Cats Effect")
  def exit1(): Nothing =
    exitIfNonZero(ReturnCode(1))
    throw new AssertionError("exit failed")

  @deprecated("Use Cats Effect")
  def exitIfNonZero(returnCode: ReturnCode): Unit =
    Log4j.shutdown()
    if returnCode.number != 0 then System.exit(returnCode.number)
