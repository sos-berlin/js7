package js7.tests.controller.proxy

import cats.effect.{IO, ResourceIO}
import java.time.Instant
import js7.base.configutils.Configs.*
import js7.base.fs2utils.StreamExtensions.takeUntil
import js7.base.log.LogLevel.Debug
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Tests.isIntelliJIdea
import js7.common.pekkoutils.Pekkos
import js7.controller.client.{HttpControllerApi, PekkoHttpControllerApi}
import js7.proxy.javaapi.{JControllerApi, JControllerProxy}
import js7.tests.controller.proxy.JLogFileTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.apache.pekko.actor.ActorSystem
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

final class JLogFileTest extends OurAsyncTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateItem ]
      }
      TEST-USER = "plain:TEST-PASSWORD"
    }
    js7.log.info.file = "${if isIntelliJIdea then "logs/test.log" else "logs/build.log"}"
    js7.log.debug.file = "${if isIntelliJIdea then "logs/test.log" else "logs/build.log"}"
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Nil
  protected val items = Nil

  "Scala" in:
    val logText = "🌶️🌶️🌶️ HELLO FROM JLogFileTest Scala! 🌶️🌶️🌶️"
    logger.info(logText)
    sleep(100.ms)

    controllerApiResource.use: controllerApi =>
      controllerApi.getLogLines(Debug, start = Instant.now.minusSeconds(3), lines = Int.MaxValue)
        .flatMap: stream =>
          stream.takeUntil: line =>
            line.contains(logText)
          .compile
          .toVector.map: lines =>
            assert(lines.size > 1 & lines.exists(_.contains(logText)))

  "Java" in :
    val logText = "🍋🍋🍋 HELLO FROM JLogFileTest Java! 🍋🍋🍋"
    logger.info(logText)
    sleep(100.ms)

    jProxyResource.use: jProxy =>
      IO.fromCompletionStage:
        IO:
          JLogFileTester.test(jProxy, logText)
      .map(_.asScala)
      .map: lines =>
        assert(lines.size > 1 & lines.exists(_.contains(logText)))
        assert(lines.forall(_.endsWith("\n")))

  "Java prettyTest" in :
    jProxyResource.use: jProxy =>
      IO.fromCompletionStage:
        IO:
          JLogFileTester.prettyTest(jProxy)
      .map: (_: Void) =>
        succeed

  private def controllerApiResource: ResourceIO[HttpControllerApi] =
    for
      given ActorSystem <- Pekkos.actorSystemResource("JLogFileTest")
      controllerApi <- PekkoHttpControllerApi.resource(controllerAdmission)
    yield
      controllerApi

  private def jProxyResource: ResourceIO[JControllerProxy] =
    for
      given ActorSystem <- Pekkos.actorSystemResource("JLogFileTest")
      jControllerApi <- JControllerApi.resource(Nel.one(controllerAdmission))
      jProxy <- jControllerApi.proxyResource()
    yield
      jProxy


object JLogFileTest:
  private val logger = Logger[this.type]
