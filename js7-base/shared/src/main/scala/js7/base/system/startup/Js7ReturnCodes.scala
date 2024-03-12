package js7.base.system.startup

import cats.effect.ExitCode
import js7.base.io.process.ReturnCode
import js7.base.utils.ProgramTermination

object Js7ReturnCodes:
  /** Emergency halt. */
  val Halt = 99
  /** Emergency halt and restart. */
  val HaltAndRestart = 98
  /** Normal shutdown and restart. */
  val Restart = 97

  def terminationToExitCode(termination: ProgramTermination): ExitCode =
    ExitCode(if termination.restart then Restart else 0)

  @deprecated
  def terminationToReturnCode(termination: ProgramTermination) =
    ReturnCode(terminationToExitCode(termination).code)
