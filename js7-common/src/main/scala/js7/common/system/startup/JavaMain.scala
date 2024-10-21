package js7.common.system.startup

import cats.effect.IO
import js7.base.system.startup.Halt
import js7.base.utils.ProgramTermination
import js7.common.message.ProblemCodeMessages

object JavaMain:

  def runMain(body: => IO[ProgramTermination]): IO[ProgramTermination] =
    IO.defer:
      initialize()
      body

  private def initialize(): Unit =
    ProblemCodeMessages.initialize()
    // Initialize class and object for possibly quicker emergency stop
    Halt.initialize()
