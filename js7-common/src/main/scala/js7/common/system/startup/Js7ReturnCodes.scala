package js7.common.system.startup

import js7.base.utils.ProgramTermination

object Js7ReturnCodes {
  /** Emergency halt. */
  val Halt = 99
  /** Emergency halt and restart. */
  val HaltAndRestart = 98
  /** Normal shutdown and restart. */
  val Restart = 97

  def terminationToExitCode(termination: ProgramTermination) =
    if (termination.restart) Restart else 0
}
