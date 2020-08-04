package js7.proxy

import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.JsonObject
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.web.HttpClient
import js7.controller.client.HttpControllerApi
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.data.item.{UpdateRepoOperation, VersionId}
import js7.data.order.FreshOrder
import js7.proxy.configuration.ProxyConf
import monix.eval.Task
import monix.reactive.Observable

final class ControllerApi(
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  proxyConf: ProxyConf = ProxyConf.default)
extends ControllerProxyWithHttp
{
  protected val apiResource: Resource[Task, HttpControllerApi] =
    apiResources.toVector.sequence.flatMap(apis =>
      Resource.liftF(
        JournaledProxy.selectActiveNodeApi(apis, onCouplingError = _.logError)))

  def startProxy(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus[ProxyEvent],
    eventBus: JournaledStateEventBus[ControllerState] = new JournaledStateEventBus[ControllerState])
  : Task[ControllerProxy] =
    ControllerProxy.start(apiResources, proxyEventBus, eventBus, proxyConf)

  def updateRepo(versionId: VersionId, operations: Observable[UpdateRepoOperation.ItemOperation]): Task[Checked[Completed]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[UpdateRepoOperation, JsonObject](
            "controller/api/repo",
            UpdateRepoOperation.AddVersion(versionId) +: operations)
            .map(_ => Completed))))

  def addOrders(orders: Observable[FreshOrder]): Task[Checked[Completed]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[FreshOrder, JsonObject]("controller/api/order", orders)
            .map(_ => Completed))))

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    execute(ControllerCommand.AddOrder(order))
      .map(_.map(o => !o.ignoredBecauseDuplicate))

  def executeCommand(command: ControllerCommand): Task[Checked[command.Response]] =
    execute(command)

  def execute(command: ControllerCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.executeCommand(command))))
}
