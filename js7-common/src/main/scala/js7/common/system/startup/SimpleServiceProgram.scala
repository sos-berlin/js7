package js7.common.system.startup

import cats.effect.{ExitCode, IO}
import js7.common.configuration.BasicConfiguration

trait SimpleServiceProgram[Cnf <: BasicConfiguration](using Cnf: BasicConfiguration.Companion[Cnf])
extends ServiceApp:

  final def run(args: List[String]): IO[ExitCode] =
    runProgramAsService(args, Cnf.fromCommandLine)(program)

  protected def program(conf: Cnf): IO[ExitCode]
