package js7.core.cluster.watch

import js7.base.generic.Completed
import js7.cluster.watch.api.AnyClusterWatch
import monix.eval.Task

trait ClusterWatchApi extends AnyClusterWatch
{
  // HttpSessionApi may implement this method
  def logout(): Task[Completed]
}
