package js7.data.cluster

import java.util.UUID
import java.util.UUID.randomUUID
import js7.base.generic.GenericString
import js7.base.utils.Base64UUID

final case class ClusterWatchRunId(base64UUID: Base64UUID) extends GenericString {
  def string = base64UUID.string
  override def toString = s"ClusterWatchRunId:$string"
}

object ClusterWatchRunId extends GenericString.Checked_[ClusterWatchRunId]
{
  val empty: ClusterWatchRunId =
    ClusterWatchRunId(Base64UUID.zero)

  def apply(uuid: UUID): ClusterWatchRunId =
    new ClusterWatchRunId(Base64UUID(uuid))

  def random(): ClusterWatchRunId =
    ClusterWatchRunId(randomUUID)

  protected def unchecked(string: String) =
    throw new NotImplementedError

  override def checked(string: String) =
    for o <- Base64UUID.checked(string) yield new ClusterWatchRunId(o)
}
