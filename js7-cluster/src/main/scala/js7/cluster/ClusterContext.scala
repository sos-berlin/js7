package js7.cluster

import cats.effect.Resource
import js7.base.web.Uri
import js7.data.cluster.ClusterNodeApi
import monix.eval.Task

private[cluster] trait ClusterContext
{
  def clusterNodeApi(uri: Uri, name: String): Resource[Task, ClusterNodeApi]
}
