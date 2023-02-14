package js7.data.cluster

import cats.instances.either.*
import cats.syntax.flatMap.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting.syntax.*
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import ClusterSetting.*
import js7.base.utils.ScalaUtils.syntax.*

final case class ClusterSetting(
  idToUri: Map[NodeId, Uri],
  activeId: NodeId,
  timing: ClusterTiming,
  clusterWatchId: Option[ClusterWatchId] = None)
{
  checkedUnit(idToUri, activeId).orThrow

  def activeUri: Uri =
    idToUri(activeId)

  def passiveId: NodeId =
    idToUri.peerOf(activeId)

  def passiveUri: Uri =
    idToUri(passiveId)

  /** Primary node should be first (possible for optimized short Scala Map). */
  def normalized: ClusterSetting =
    copy(idToUri =
      idToUri
        .toVector
        .sortBy(o => if (o._1 == activeId) 0 else 1)
        .toMap)

  def withPassiveUri(uri: Uri): ClusterSetting =
    copy(idToUri = idToUri + (passiveId -> uri))

  @TestOnly
  def other(nodeId: NodeId): NodeId = {
    if (nodeId == activeId) passiveId
    else if (nodeId == passiveId) activeId
    else throw new IllegalArgumentException(s"Unknown $nodeId")
  }
}

object ClusterSetting
{
  def checked(
    idToUri: Map[NodeId, Uri],
    activeId: NodeId,
    timing: ClusterTiming,
    clusterWatchId: Option[ClusterWatchId])
  : Checked[ClusterSetting] =
    for (_ <- checkedUnit(idToUri, activeId)) yield
      new ClusterSetting(idToUri, activeId, timing, clusterWatchId)

  private def checkedUnit(idToUri: Map[NodeId, Uri], activeId: NodeId) =
    checkUris(idToUri) >>
      (if (!idToUri.contains(activeId))
        Left(Problem(
          s"Unknown $activeId, expected one of ${idToUri.keys.mkString("'", "', '", "'")}"))
      else
        Right(()))

  private[cluster] def checkUris(idToUri: Map[NodeId, Uri]): Checked[idToUri.type] =
    if (idToUri.size != 2)
      Left(Problem("Exactly two URIs are expected"))
    else if (idToUri.values.toVector.distinct.size != idToUri.size)
      Left(Problem("URIs must be different"))
    else
      Right(idToUri)

  object syntax {
    implicit final class RichIdToUri(private val idToUri: Map[NodeId, Uri]) extends AnyVal {
      def peerOf(id: NodeId): NodeId = {
        assertThat(idToUri.keySet contains id)
        idToUri.keys.filter(_ != id).head
      }
    }
  }

  implicit val jsonCodec: Codec.AsObject[ClusterSetting] = deriveCodec
}
