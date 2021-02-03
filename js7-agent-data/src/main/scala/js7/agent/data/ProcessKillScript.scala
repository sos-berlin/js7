package js7.agent.data

import java.nio.file.{Path, Paths}
import js7.base.generic.GenericString
import js7.common.process.Processes.Pid

/**
  * @author Joacim Zschimmer
  */
final case class ProcessKillScript(file: Path) extends GenericString {
  def string = file.toString

  def toCommandArguments(id: AgentTaskId, pid: Option[Pid]): Seq[String] =
    Vector(file.toString, s"--kill-agent-task-id=${id.string}") ++ (pid map { o => s"--pid=${o.string}" })
}

object ProcessKillScript extends GenericString.Companion[ProcessKillScript]
{
  override def apply(o: String) = new ProcessKillScript(Paths.get(o))
}
