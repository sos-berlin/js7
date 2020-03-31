package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.web.Uri

object ClusterSetting
{
  private type Id = ClusterNodeId

  def checkUris(idToUri: Map[Id, Uri], id: Id): Checked[Unit] =
    checkUris(idToUri) >>
       (if (!idToUri.contains(id))
         Left(Problem(s"Unknown ClusterNodeId: '$id', expected one of ${idToUri.keys.mkString("'", "', '", "'")}"))
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
    implicit final class RichIdToUri(private val idToUri: Map[ClusterNodeId, Uri]) extends AnyVal {
      def peerOf(id: Id): Id =
        idToUri.keys.filter(_ != id).head
    }
  }
}

