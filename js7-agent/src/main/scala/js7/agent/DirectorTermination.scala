package js7.agent

import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichBoolean

sealed trait DirectorTermination extends ProgramTermination:

  def restartJvm: Boolean

  def restartDirector: Boolean

  assert(restart == restartJvm | restartDirector)

  def copy(restartJvm: Boolean = this.restartJvm, restartDirector: Boolean = this.restartDirector): DirectorTermination =
    DirectorTermination(restartJvm, restartDirector)

  override def toString =
    s"DirectorTermination(${
      ((restartJvm ? "restartJvm") ++ (restartDirector ? "restartDirector")).mkString(" ")
    })"


object DirectorTermination:

  def apply(restartJvm: Boolean = false, restartDirector: Boolean = false): DirectorTermination =
    if restartJvm | restartDirector then
      Restart(restartJvm, restartDirector)
    else
      Success(restartJvm, restartDirector)

  def fromProgramTermination(termination: ProgramTermination): DirectorTermination =
    DirectorTermination(restartJvm = termination.restart)


  private case class Success(restartJvm: Boolean, restartDirector: Boolean)
  extends DirectorTermination with ProgramTermination.Success

  private case class Restart(restartJvm: Boolean, restartDirector: Boolean)
  extends DirectorTermination with ProgramTermination.Restart
