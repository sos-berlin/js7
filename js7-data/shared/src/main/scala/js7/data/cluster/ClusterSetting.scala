package js7.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import io.circe.generic.semiauto.deriveCodec
import js7.base.annotation.javaApi
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting._
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.node.NodeId

final case class ClusterSetting private(
  idToUri: Map[NodeId, Uri],
  activeId: NodeId,
  clusterWatches: Seq[ClusterSetting.Watch],
  timing: ClusterTiming)
{
  checkedUnit(idToUri, activeId, clusterWatches).orThrow

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

  def clusterWatchUri: Uri = {
    assertThat(clusterWatches.sizeIs == 1)
    clusterWatches.head.uri
  }

  def clusterWatchUris =
    clusterWatches.map(_.uri)

  def withPassiveUri(uri: Uri): ClusterSetting =
    copy(idToUri = idToUri + (passiveId -> uri))
}

object ClusterSetting
{
  def checked(
    idToUri: Map[NodeId, Uri],
    activeId: NodeId,
    clusterWatches: Seq[Watch],
    timing: ClusterTiming)
  : Checked[ClusterSetting] =
    for (_ <- checkedUnit(idToUri, activeId, clusterWatches)) yield
      new ClusterSetting(idToUri, activeId, clusterWatches, timing)

  private def checkedUnit(idToUri: Map[NodeId, Uri], activeId: NodeId, clusterWatches: Seq[Watch]) =
    checkUris(idToUri) >>
      (if (!idToUri.contains(activeId))
        Left(Problem(s"Unknown NodeId: '$activeId', expected one of ${idToUri.keys.mkString("'", "', '", "'")}"))
      else if (clusterWatches.sizeIs != 1)
        Left(Problem.pure("Exactly one cluster watch URI is required"))
      else
        Right(()))

  private[cluster] def checkUris(idToUri: Map[NodeId, Uri]): Checked[idToUri.type] =
    if (idToUri.size != 2)
      Left(Problem("Exactly two URIs are expected"))
    else if (idToUri.values.toVector.distinct.size != idToUri.size)
      Left(Problem("URIs must be different"))
    else
      Right(idToUri)

  final case class Watch(uri: Uri)
  object Watch {
    @javaApi
    def of(uri: Uri) = Watch(uri)

    implicit val jsonCodec = deriveCodec[Watch]
  }

  object syntax {
    implicit final class RichIdToUri(private val idToUri: Map[NodeId, Uri]) extends AnyVal {
      def peerOf(id: NodeId): NodeId = {
        assertThat(idToUri.keySet contains id)
        idToUri.keys.filter(_ != id).head
      }
    }
  }

  implicit val jsonCodec = deriveCodec[ClusterSetting]
}
