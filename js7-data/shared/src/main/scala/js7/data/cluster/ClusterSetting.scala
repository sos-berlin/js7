package js7.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.node.NodeId

final case class ClusterSetting private(
  idToUri: Map[NodeId, Uri],
  activeId: NodeId)
{
  def passiveId: NodeId =
    idToUri.peerOf(activeId)

  /** Primary node should be first (possible for optimized short Scala Map). */
  def normalized: ClusterSetting =
    copy(idToUri =
      idToUri
        .toVector
        .sortBy(o => if (o._1 == activeId) 0 else 1)
        .toMap)

  @throws[AssertionError]
  def assertIsValid(): Unit =
    ClusterSetting.checked(idToUri, activeId)
      .orThrow
}

object ClusterSetting
{
  private type Id = NodeId

  def unchecked(idToUri: Map[Id, Uri], activeId: Id): ClusterSetting =
    checked(idToUri, activeId).orThrow

  def checked(idToUri: Map[Id, Uri], activeId: Id): Checked[ClusterSetting] =
    checkUris(idToUri) >>
      (if (!idToUri.contains(activeId))
        Left(Problem(s"Unknown NodeId: '$activeId', expected one of ${idToUri.keys.mkString("'", "', '", "'")}"))
      else
        Right(new ClusterSetting(idToUri, activeId)))

  def checkUris(idToUri: Map[Id, Uri]): Checked[idToUri.type] =
    if (idToUri.size != 2)
      Left(Problem("Exactly two URIs are expected"))
    else if (idToUri.values.toVector.distinct.size != idToUri.size)
      Left(Problem("URIs must be different"))
    else
      Right(idToUri)

  object syntax {
    implicit final class RichIdToUri(private val idToUri: Map[NodeId, Uri]) extends AnyVal {
      def peerOf(id: Id): Id =
        idToUri.keys.filter(_ != id).head
    }
  }

  implicit val jsonCodec = deriveCodec[ClusterSetting]
}

