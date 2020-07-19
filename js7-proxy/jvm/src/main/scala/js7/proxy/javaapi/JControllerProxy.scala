package js7.proxy.javaapi

import cats.data.EitherT
import cats.effect.Resource
import io.circe.Json
import io.vavr.control.{Either => VEither}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJson}
import js7.base.problem.Problem
import js7.controller.client.HttpControllerApi
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.data.event.Event
import js7.proxy.javaapi.data.{JControllerCommand, JControllerState, JFreshOrder}
import js7.proxy.javaapi.utils.VavrConversions._
import js7.proxy.{ControllerCommandProxy, JournaledProxy, ProxyEvent}
import monix.eval.Task
import monix.execution.FutureUtils.Java8Extensions
import reactor.core.publisher.Flux

/** Java adapter for `JournaledProxy[JControllerState]`. */
@javaApi
final class JControllerProxy private[proxy](
  journaledProxy: JournaledProxy[ControllerState],
  val proxyEventBus: JStandardEventBus[ProxyEvent],
  val controllerEventBus: JControllerEventBus,
  apiResource: Resource[Task, HttpControllerApi],
  context: JProxyContext)
{
  import context.scheduler

  private val commandProxy = new ControllerCommandProxy(apiResource)

  def startObserving: CompletableFuture[Unit] =
    journaledProxy.startObserving
      .runToFuture
      .asJava

  def flux: Flux[JEventAndControllerState[Event]] =
    Flux.from(
      journaledProxy.observe
        .map(JEventAndControllerState.fromScala)
        .toReactivePublisher)

  def stop: CompletableFuture[java.lang.Void] =
    journaledProxy.stop
      .map(_ => null: java.lang.Void)
      .runToFuture
      .asJava

  def currentState: JControllerState =
    JControllerState(journaledProxy.currentState._2)

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
    commandProxy.execute(ControllerCommand.AddOrder(order.underlying))
      .map(_
        .map(o => java.lang.Boolean.valueOf(!o.ignoredBecauseDuplicate))
        .toVavr)
      .runToFuture
      .asJava

  def executeCommand(command: JControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    commandProxy.execute(command.underlying)
      .map(_
        .map(o => (o: ControllerCommand.Response))
        .toVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", command)

  def httpPostJson(uriTail: String, jsonString: String): CompletableFuture[VEither[Problem, String]] =
    (for {
      requestJson <- EitherT(Task(io.circe.parser.parse(jsonString).toChecked))
      responseJson <- EitherT(
        apiResource.use(api =>
          api.login(onlyIfNotLoggedIn = true) >>
            api.httpClient.liftProblem(
              api.post[Json, Json](uriTail, requestJson))))
    } yield responseJson).value
      .map(_
        .map(_.compactPrint)
        .toVavr)
      .runToFuture
      .asJava

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  def httpGetJson(uriTail: String): CompletableFuture[VEither[Problem, String]] =
    apiResource
      .use(api =>
        api.login(onlyIfNotLoggedIn = true) >>
          api.httpClient.liftProblem(
            api.get[Json](uriTail)))
      .map(_
        .map(_.compactPrint)
        .toVavr)
      .runToFuture
      .asJava
}
