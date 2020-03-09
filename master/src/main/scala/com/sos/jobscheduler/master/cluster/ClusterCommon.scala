package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.eventbus.EventBus
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.cluster.ClusterWatch.ClusterWatchHeartbeatFromInactiveNodeProblem
import com.sos.jobscheduler.core.cluster.ClusterWatchApi
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.master.cluster.ClusterCommon._
import com.sos.jobscheduler.master.cluster.PassiveClusterNode.{AgentAgreesToActivation, AgentDoesNotAgreeToActivation}
import monix.eval.Task

private[cluster] final class ClusterCommon(
  ownUri: Uri,
  activationInhibitor: ActivationInhibitor,
  val clusterWatch: ClusterWatchApi,
  testEventBus: EventBus)
{
  private[cluster] def ifClusterWatchAllowsActivation[A](clusterState: ClusterState, event: ClusterEvent, body: Task[Checked[Boolean]])
  : Task[Checked[Boolean]] =
    activationInhibitor.tryToActivate(
      ifInhibited = Task.pure(Right(false)),  // Ignore heartbeat loss
      activate =
        clusterState.applyEvent(event) match {
          case Left(problem) => Task.pure(Left(problem))
          case Right(updatedClusterState) =>
            clusterWatch.applyEvents(from = ownUri, event :: Nil, updatedClusterState).flatMap {
              case Left(problem) =>
                if (problem.codeOption contains ClusterWatchHeartbeatFromInactiveNodeProblem.code) {
                  logger.info(s"ClusterWatch did not agree to failover: $problem")
                  testEventBus.publish(AgentDoesNotAgreeToActivation)
                  Task.pure(Right(false))  // Ignore heartbeat loss
                } else
                  Task.pure(Left(problem))

              case Right(Completed) =>
                logger.info(s"ClusterWatch agreed to failover")
                testEventBus.publish(AgentAgreesToActivation)
                body
            }
        }
    )
}

object ClusterCommon {
  private val logger = Logger(getClass)
}
