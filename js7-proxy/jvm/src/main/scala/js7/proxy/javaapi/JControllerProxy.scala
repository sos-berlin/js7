package js7.proxy.javaapi

import cats.data.EitherT
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import io.vavr.control.{Either => VEither}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJson}
import js7.base.problem.Problem
import js7.base.utils.Closer
import js7.base.web.Uri
import js7.common.configuration.JobSchedulerConfiguration
import js7.common.configutils.Configs
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.Logger
import js7.common.system.ThreadPools
import js7.common.utils.JavaResource
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.proxy.javaapi.JControllerProxy._
import js7.proxy.javaapi.data.{JControllerState, JHttpsConfig}
import js7.proxy.javaapi.utils.VavrConversions._
import js7.proxy.{ControllerCommandProxy, JournaledProxy}
import monix.eval.Task
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.schedulers.ExecutorScheduler

/** Java adapter for `JournaledProxy[JControllerState]`. */
@javaApi
final class JControllerProxy private(
  journaledProxy: JournaledProxy[ControllerState],
  val eventBus: JProxyEventBus,
  apiResource: Resource[Task, HttpControllerApi],
  closer: Closer)
  (implicit scheduler: ExecutorScheduler)
extends AutoCloseable
{
  private val commandProxy = new ControllerCommandProxy(apiResource)

  /** If stop() has been called and completed before, this call will not block the thread. */
  def close() = {
    logger.debug("close() ...")
    stop().get()/*blocking*/
    closer.close()
    logger.debug("close() finished")
  }

  def stop(): CompletableFuture[java.lang.Void] =
    journaledProxy.stop
      .map(_ => null: java.lang.Void)
      .runToFuture
      .asJava

  def currentState: JControllerState =
    new JControllerState(journaledProxy.currentState._2)

  def executeCommand(command: ControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    commandProxy.execute(command)
      .map(_
        .map(o => (o: ControllerCommand.Response))
        .asVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPost("/controller/api/command", command)

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
object JControllerProxy
{
  coupleScribeWithSlf4j()

  private val logger = Logger(getClass)

  private val defaultConfig =
    Configs.loadResource(JavaResource("js7/proxy/configuration/proxy.conf"), internal = true)
      .withFallback(JobSchedulerConfiguration.defaultConfig)

  def start(uri: String, credentials: JCredentials, httpsConfig: JHttpsConfig)
  : CompletableFuture[JControllerProxy] =
    start(uri, credentials, httpsConfig, new JProxyEventBus, ConfigFactory.empty)

  def start(uri: String, credentials: JCredentials, httpsConfig: JHttpsConfig, eventBus: JProxyEventBus, config: Config)
  : CompletableFuture[JControllerProxy] = {
    val closer = new Closer
    implicit val scheduler = ThreadPools.newStandardScheduler("JControllerProxy", config withFallback defaultConfig, closer)

    val apiResource = AkkaHttpControllerApi.separateAkkaResource(Uri(uri), credentials.toUnderlying,
      httpsConfig.toScala, config = config)

    JournaledProxy.start[ControllerState](apiResource, eventBus.underlying.publish)
      .map(proxy => new JControllerProxy(proxy, eventBus, apiResource, closer))
      .runToFuture
      .asJava
  }
}
