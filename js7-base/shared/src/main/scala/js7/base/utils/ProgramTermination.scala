package js7.base.utils

import cats.effect.ExitCode
import js7.base.system.startup.Js7ReturnCodes
import js7.base.system.startup.Js7ReturnCodes.Restart
import js7.base.utils.ScalaUtils.syntax.*

trait ProgramTermination:
  def restart: Boolean = false

  def toExitCode: ExitCode

  override def toString = s"ProgramTermination(${restart ?? "restart"})"


object ProgramTermination:

  def apply(restart: Boolean = false): ProgramTermination =
    if restart then
      Restart
    else
      Success


  trait Success extends ProgramTermination:
    def toExitCode = ExitCode.Success

    override def toString = s"ProgramTermination.Success"

  case object Success extends Success


  trait Failure extends ProgramTermination:
    def toExitCode = ExitCode.Error

    override def toString = "ProgramTermination.Failure"

  case object Failure extends Failure


  trait Restart extends ProgramTermination:
    val toExitCode = ExitCode(Js7ReturnCodes.Restart)

    override def restart = true

    override def toString = "ProgramTermination.Restart"

  case object Restart extends Restart
