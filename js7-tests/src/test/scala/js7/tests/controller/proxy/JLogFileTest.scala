package js7.tests.controller.proxy

import cats.effect.{IO, ResourceIO}
import cats.syntax.traverse.*
import java.nio.file.{Files, Paths}
import java.time.Instant
import java.util.Arrays.asList
import java.util.concurrent.{CompletableFuture, ForkJoinPool}
import js7.base.configutils.Configs.*
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.takeUntil
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.LogLevel.Debug
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTime.extensions.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Tests.isIntelliJIdea
import js7.common.pekkoutils.Pekkos
import js7.controller.client.{HttpControllerApi, PekkoHttpControllerApi}
import js7.data_for_java.auth.JAdmission
import js7.proxy.javaapi.{JControllerApi, JControllerProxy, JProxyContext}
import js7.tests.controller.proxy.JLogFileTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.apache.pekko.actor.ActorSystem
import org.scalatest.Assertion
import scala.concurrent.duration.Deadline
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

final class JLogFileTest extends OurAsyncTestSuite, ControllerAgentForScalaTest:

  private val debugLogFile = Paths.get:
    if isIntelliJIdea then "logs/test.log" else "logs/build.log"

  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateItem ]
      }
      TEST-USER = "plain:TEST-PASSWORD"
    }
    js7.log.info.file  = "${if isIntelliJIdea then "logs/test.log" else "logs/build.log"}"
    js7.log.debug.file = "$debugLogFile"
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Nil
  protected val items = Nil

  private val jAdmissions = asList(JAdmission(controllerAdmission))

  "Scala" in:
    val logText = "🌶️🌶️🌶️ HELLO FROM JLogFileTest Scala! 🌶️🌶️🌶️"
    logger.info(logText)
    controllerApiResource.use: controllerApi =>
      controllerApi.getLogLines(Debug, begin = Instant.now - 3.s, lines = Int.MaxValue)
        .flatMap: stream =>
          stream.map(_.utf8String)
            .takeUntil: line =>
              line.contains(logText)
            .compile
            .toVector.map: lines =>
              assert(lines.size >= 1 & lines.exists(_.contains(logText)))

  "Java" in :
    val logText = "🍋🍋🍋 HELLO FROM JLogFileTest Java! 🍋🍋🍋"
    logger.info(logText)
    // Java-like code, but JLogFileTester.test runs in Proxy's thread:
    JControllerApi.run(admissions = controllerAdmission :: Nil): jControllerApi =>
      jControllerApi.runControllerProxy: jControllerProxy =>
        JLogFileTester.test(jControllerProxy, logText)
    .map: result =>
      val lines = result.firstList().asScala
      assert(lines.size >= 1 & lines.exists(_.contains(logText)))
      assert(lines.forall(_.endsWith("\n")))

  "Java prettyTestNonBlocking" in :
    IO.fromCompletionStage:
      IO:
        JLogFileTester.prettyTestNonBlocking(jAdmissions)
    .evalOnExecutor(ForkJoinPool.commonPool)
    .map: n =>
      assert(n >= 1)

  "Java prettyTestBlocking" in :
    IO.fromCompletableFuture:
      IO:
        JLogFileTester.prettyTestBlocking(jAdmissions)
    .evalOnExecutor(ForkJoinPool.commonPool)
    .map: n =>
      assert(n >= 1)

  "Speed tests" - {
    val logFile = debugLogFile
    lazy val fillLog =
      val expectedSize = 200_000_000L
      val line = "FILLER " + "+" * 200
      (1L to (expectedSize - Files.size(logFile)) / (90 + line.length)).foreach: i =>
        logger.debug(line)

    "Non blocking" in:
      runMyTest("Non blocking: "):
        JLogFileTester.prettyTestNonBlocking

    "Blocking" in:
      runMyTest("Blocking: "):
        JLogFileTester.prettyTestBlocking

    "Non blocking raw lines" in:
      runMyTest("Non blocking raw: "):
        JLogFileTester.testRawNonBlocking

    def runMyTest(prefix: String, n: Int = 4)
      (body: JControllerProxy => CompletableFuture[java.lang.Long])
    : IO[Assertion] =
      if !isIntelliJIdea then
        IO.pure(pending)
      else
        fillLog
        val size = Files.size(logFile)
        JProxyContext.resource().use: jProxyContext =>
          jProxyContext.controllerApiResource(Nel.one(controllerAdmission)).use: jControllerApi =>
            jControllerApi.runControllerProxyIO: jControllerProxy =>
              (1 to n).toVector.traverse: _ =>
                IO.defer:
                  val t = Deadline.now
                  IO.fromCompletableFuture:
                    IO:
                      body(jControllerProxy)
                    .evalOn(jProxyContext.given_IORuntime.compute)
                  .map: lineCount =>
                    val elapsed = t.elapsed
                    logger.info(bold(prefix + bytesPerSecondString(elapsed, size)))
                    logger.info(bold(prefix + itemsPerSecondString(elapsed, lineCount, "lines")))
                    lineCount
              .map(_.last) // Line count of last run
            .evalOn(jProxyContext.given_IORuntime.compute) // Due to assertion in the JLogFileTester
        .as(succeed)
  }

  private def controllerApiResource: ResourceIO[HttpControllerApi] =
    for
      given ActorSystem <- Pekkos.actorSystemResource("JLogFileTest")
      controllerApi <- PekkoHttpControllerApi.resource(controllerAdmission)
    yield
      controllerApi


object JLogFileTest:
  private val logger = Logger[this.type]
