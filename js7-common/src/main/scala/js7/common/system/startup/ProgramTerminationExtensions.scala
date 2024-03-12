package js7.common.system.startup

import cats.effect.ExitCode
import js7.base.system.startup.Js7ReturnCodes
import js7.base.utils.ProgramTermination

object ProgramTerminationExtensions:

  extension (termination: ProgramTermination)
    def toExitCode: ExitCode =
      Js7ReturnCodes.terminationToExitCode(termination)
