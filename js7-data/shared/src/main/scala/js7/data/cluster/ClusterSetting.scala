package js7.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import js7.base.problem.{Checked, Problem}
import js7.base.web.Uri
import js7.data.node.NodeId

object ClusterSetting
{
  private type Id = NodeId

  def checkUris(idToUri: Map[Id, Uri], id: Id): Checked[Unit] =
    checkUris(idToUri) >>
       (if (!idToUri.contains(id))
         Left(Problem(s"Unknown NodeId: '$id', expected one of ${idToUri.keys.mkString("'", "', '", "'")}"))
       else
         Checked.unit)

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
}

