package js7.cluster

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.cluster.ClusterWatchUniquenessChecker.*
import js7.data.cluster.ClusterWatchProblems.ClusterWatchIdNotUniqueProblem
import js7.data.cluster.{ClusterWatchId, ClusterWatchRunId}
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable

final class ClusterWatchUniquenessChecker(memorySize: Int):

  require(memorySize >= 1)

  private val usedRunIds = mutable.Queue.empty[ClusterWatchRunId]
  private val isUsedRunId = mutable.Set.empty[ClusterWatchRunId]
  private var idToRunId = Map.empty[ClusterWatchId, ClusterWatchRunId]

  def check(clusterWatchId: ClusterWatchId, clusterWatchRunId: ClusterWatchRunId): Checked[Unit] =
    synchronized:
      if idToRunId.get(clusterWatchId) contains clusterWatchRunId then
        Checked.unit
      else if idToRunId.values.exists(_ == clusterWatchRunId) || isUsedRunId(clusterWatchRunId) then
        // Only in the weird case, that the same clusterWatchRunId has another clusterWatchId
        val problem = ClusterWatchIdNotUniqueProblem(clusterWatchId, clusterWatchRunId)
        logger.warn(problem.toString)
        Left(problem)
      else
        logger.debug(s"Remember $clusterWatchId $clusterWatchRunId")
        idToRunId = idToRunId.updated(clusterWatchId, clusterWatchRunId)
        if usedRunIds.length >= memorySize then
          isUsedRunId -= usedRunIds.dequeue()
        usedRunIds += clusterWatchRunId
        isUsedRunId += clusterWatchRunId
        Checked.unit

  @TestOnly private[cluster] def test(
    clusterWatchId: ClusterWatchId,
    clusterWatchRunId: ClusterWatchRunId)
  : Checked[Unit] =
    val result = check(clusterWatchId, clusterWatchRunId)
    assert(usedRunIds.sizeIs == isUsedRunId.size
      && usedRunIds.toSet == isUsedRunId
      && idToRunId.values.toSet.size == idToRunId.size)
    result

object ClusterWatchUniquenessChecker:
  private val logger = Logger[this.type]
