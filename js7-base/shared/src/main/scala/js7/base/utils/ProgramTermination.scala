package js7.base.utils

import cats.effect.ExitCode
import js7.base.system.startup.Js7ReturnCodes
import js7.base.utils.ScalaUtils.syntax.*

trait ProgramTermination:
  def restart: Boolean = false

  def toExitCode: ExitCode

  override def toString = s"ProgramTermination($toExitCode${restart ?? " restart"})"


object ProgramTermination:

  def apply(restart: Boolean = false): ProgramTermination =
    if restart then
      Restart
    else
      Success

  def fromExitCode(exitCode: ExitCode): ProgramTermination =
    if exitCode == ExitCode.Success then
      ProgramTermination.Success
    else
      FromExitCode(exitCode)

  def fromUnitOrExitCode(exitCode: Unit | ExitCode | ProgramTermination): ProgramTermination =
    exitCode match
      case () => Success
      case exitCode: ExitCode => fromExitCode(exitCode)
      case o: ProgramTermination => o

  trait Success extends ProgramTermination:
    def toExitCode: ExitCode =
      ExitCode.Success

    override def toString =
      s"ProgramTermination.Success"

  case object Success extends Success


  trait Failure extends ProgramTermination:
    def toExitCode: ExitCode =
      ExitCode.Error

    override def toString = "ProgramTermination.Failure"

  case object Failure extends Failure


  trait Restart extends ProgramTermination:
    val toExitCode: ExitCode =
      ExitCode(Js7ReturnCodes.Restart)

    override def restart = true

    override def toString = "ProgramTermination.Restart"

  case object Restart extends Restart


  final case class FromExitCode(toExitCode: ExitCode) extends ProgramTermination:
    override def toString = s"ProgramTermination.FromExitCode($toExitCode)"
