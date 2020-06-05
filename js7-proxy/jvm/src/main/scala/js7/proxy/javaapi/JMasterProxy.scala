package js7.proxy.javaapi

import cats.data.EitherT
import cats.effect.Resource
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJson}
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.common.scalautil.Logger
import js7.data.event.Event
import js7.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import js7.master.data.{MasterCommand, MasterState}
import js7.proxy.javaapi.JMasterProxy._
import js7.proxy.javaapi.data.JMasterState
import js7.proxy.javaapi.utils.VavrConversions._
import js7.proxy.{JournaledProxy, MasterCommandProxy}
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import io.vavr.control.{Either => VEither}
import java.util.concurrent.CompletableFuture
import monix.eval.Task
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.schedulers.ExecutorScheduler

/** Java adapter for `JournaledProxy[JMasterState]`. */
@javaApi
final class JMasterProxy private(
  journaledProxy: JournaledProxy[MasterState],
  val eventBus: JProxyEventBus,
  apiResource: Resource[Task, HttpMasterApi])
  (implicit scheduler: ExecutorScheduler)
extends AutoCloseable
{
  private val commandProxy = new MasterCommandProxy(apiResource)

  //eventBus.underlying.subscribe[Event](e => _currentState = e.state)

  /** If stop() has been called before, this call will not block the thread. */
  def close() = {
    logger.debug("close() ...")
    stop().get()/*blocking*/
    scheduler.shutdown()
    logger.debug("close() finished")
  }

  def stop(): CompletableFuture[java.lang.Void] =
    journaledProxy.stop
      .map(_ => null: java.lang.Void)
      .runToFuture
      .asJava

  def currentState: JMasterState =
    new JMasterState(journaledProxy.currentState._2)

  def executeCommand(command: MasterCommand): CompletableFuture[VEither[Problem, MasterCommand.Response]] =
    commandProxy.execute(command)
      .map(_
        .map(o => (o: MasterCommand.Response))
        .asVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPost("/master/api/command", command)

  def httpPost(uriTail: String, jsonString: String): CompletableFuture[VEither[Problem, String]] =
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
        .asVavr)
      .runToFuture
      .asJava
}

@javaApi
object JMasterProxy
{
  private val logger = Logger(getClass)
  private val MaxThreads = 100  // Some limit, just in case

  def start(uri: String, credentials: JCredentials)
  : CompletableFuture[JMasterProxy] =
    start(uri, credentials, new JProxyEventBus, ConfigFactory.empty)

  def start(uri: String, credentials: JCredentials, eventBus: JProxyEventBus, config: Config)
  : CompletableFuture[JMasterProxy] = {
    implicit val scheduler = JThreadPools.newStandardScheduler(
      parallelism = sys.runtime.availableProcessors, maxThreads = MaxThreads, name = "JMasterProxy")

    val apiResource = AkkaHttpMasterApi.separateAkkaResource(Uri(uri), credentials.toUnderlying, config)

    JournaledProxy.start[MasterState](apiResource, eventBus.underlying.publish)
      .map(proxy => new JMasterProxy(proxy, eventBus, apiResource)(scheduler))
      .runToFuture
      .asJava
  }
}
