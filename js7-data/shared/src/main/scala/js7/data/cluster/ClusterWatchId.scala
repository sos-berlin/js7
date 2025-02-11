package js7.data.cluster

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.data.agent.AgentPath.mayThrow

final case class ClusterWatchId private(string: String) extends GenericString:
  override def toString = s"ClusterWatch:$string"


object ClusterWatchId extends GenericString.NonEmpty[ClusterWatchId]:
  @javaApi
  def of(string: String): ClusterWatchId =
    mayThrow(string)

  protected def unchecked(string: String) =
    new ClusterWatchId(string)
