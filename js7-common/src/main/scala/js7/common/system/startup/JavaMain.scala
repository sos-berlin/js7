package js7.common.system.startup

import cats.effect.{ExitCode, IO}
import js7.base.log.Logger
import js7.base.system.startup.StartUp.printlnWithClock
import js7.base.system.startup.Halt
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
        .recover: t =>
          logger.error(t.toStringWithCauses, t)
          printlnWithClock(s"TERMINATING DUE TO ERROR: ${t.toStringWithCauses}")
          ExitCode(1)

  private def initialize(): Unit =
    ProblemCodeMessages.initialize()
    // Initialize class and object for possibly quicker emergency stop
    Halt.initialize()
