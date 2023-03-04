package js7.controller.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.data.controller.ControllerState
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import scala.concurrent.duration.Deadline

object ControllerWebServer
{
  def resource(
    orderApi: OrderApi,
    commandExecutor: ControllerCommandExecutor,
    itemUpdater: ItemUpdater,
    controllerState: Task[Checked[ControllerState]],
    clusterNode: ClusterNode[ControllerState],
    totalRunningSince: Deadline,
    eventWatch: FileEventWatch,
    controllerConfiguration: ControllerConfiguration,
    sessionRegister: SessionRegister[SimpleSession])(
    implicit actorSystem_ : ActorSystem)
  : Resource[Task, ControllerWebServer] =
    AkkaWebServer.resource(
      controllerConfiguration.webServerBindings,
      controllerConfiguration.config,
      (binding, whenShuttingDown) =>
        Task.deferAction(implicit scheduler => Task.pure(
          new ControllerBoundRoute(
            binding,
            whenShuttingDown,
            controllerConfiguration,
            orderApi,
            commandExecutor,
            itemUpdater,
            controllerState,
            clusterNode,
            totalRunningSince,
            sessionRegister,
            eventWatch))))
}
