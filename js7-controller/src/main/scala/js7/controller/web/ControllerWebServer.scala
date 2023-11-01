package js7.controller.web

import com.google.inject.Injector
import javax.inject.{Inject, Singleton}
import js7.base.problem.Checked
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.cluster.ClusterNode
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.data.controller.ControllerState
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.Deadline

object ControllerWebServer
{
  @Singleton
  final class Factory @Inject private(
    controllerConfiguration: ControllerConfiguration,
    sessionRegister: SessionRegister[SimpleSession],
    injector: Injector,
    closer: Closer)(
    implicit actorSystem_ : ActorSystem)
  {
    def apply(
      orderApi: OrderApi,
      commandExecutor: ControllerCommandExecutor,
      itemUpdater: ItemUpdater,
      controllerState: Task[Checked[ControllerState]],
      clusterNode: ClusterNode[ControllerState],
      totalRunningSince: Deadline,
      eventWatch: FileEventWatch)
    : PekkoWebServer & PekkoWebServer.HasUri =
      new PekkoWebServer.Standard(
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
              eventWatch,
              injector)))
      ).closeWithCloser(closer)
  }
}
