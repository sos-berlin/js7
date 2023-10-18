package js7.agent

import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichBoolean

final case class DirectorTermination(
  restartJvm: Boolean = false,
  restartDirector: Boolean = false)
extends ProgramTermination:
  override val restart = restartJvm | restartDirector

  override def toString =
    s"DirectorTermination(${
      ((restartJvm ? "restartJvm") ++ (restartDirector ? "restartDirector")).mkString(" ")
    })"


object DirectorTermination:
  def fromProgramTermination(termination: ProgramTermination): DirectorTermination =
    DirectorTermination(restartJvm = termination.restart)
