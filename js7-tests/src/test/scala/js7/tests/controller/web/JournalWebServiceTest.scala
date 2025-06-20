package js7.tests.controller.web

import cats.effect.IO
import java.nio.file.Files
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.onErrorEvalTap
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.toListL
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient.HttpException
import js7.controller.client.PekkoHttpControllerApi
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.{AgentDedicated, AgentEventsObserved, AgentReady}
import js7.data.controller.ControllerCommand
import js7.data.event.JournalEvent
import js7.data.event.JournalSeparators.EndOfJournalFileMarker
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.controller.web.JournalWebServiceTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import org.apache.pekko.http.scaladsl.model.StatusCodes.Unauthorized
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable

final class JournalWebServiceTest extends OurTestSuite, BeforeAndAfterAll, ControllerAgentForScalaTest:

  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(workflow)
  private lazy val uri = controller.localUri
  private lazy val httpControllerApi = new PekkoHttpControllerApi(
    uri,
    Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))),
    controller.actorSystem
  ).closeWithCloser
  private lazy val httpClient = httpControllerApi.httpClient

  import httpControllerApi.implicitSessionToken

  override def beforeAll() =
    super.beforeAll()
    directoryProvider.agentEnvs(0).writeExecutable(pathExecutable, script(3.s))

  override protected val controllerConfig = config"""
    js7.auth.users {
      TEST-USER {
        password = "plain:TEST-PASSWORD",
      }
    }
    js7.journal.remove-obsolete-files = false"""

  "/controller/api/journal requires authentication" in:
    val e = intercept[HttpException]:
      httpClient.getDecodedLinesStream[String](Uri(s"$uri/controller/api/journal?file=0&position=0")).await(99.s)
    assert(e.status == Unauthorized)

  "Login" in:
    httpControllerApi.login().await(99.s)

  "/controller/api/journal" in:
    pending // TODO Duplicate with JournalRouteTest, active node does not provide event acknowledgements

    var replicated = ByteArray.empty
    controller.eventWatch.await[AgentReady](_ => true)  // Await last event

    val whenReplicated = httpClient.getRawLinesStream(Uri(s"$uri/controller/api/journal?markEOF=true&file=0&position=0"))
      .await(99.s)
      .foreach(o => IO:
        replicated ++= o)
      .compile
      .drain
      .unsafeToFuture()

    val observedLengths = mutable.Buffer[String]()
    val whenLengthsObserved = httpClient.getRawLinesStream(Uri(s"$uri/controller/api/journal?markEOF=true&file=0&position=0&return=ack"))
      .await(99.s)
      .foreach(o => IO:
        observedLengths += o.utf8String)
      .compile.drain
      .unsafeToFuture()

    val stateDir = controller.conf.stateDirectory
    val controller0File = stateDir / "controller--0.journal"
    awaitAndAssert(replicated.length == Files.size(controller0File))
    //assert(replicated.utf8String endsWith EndOfJournalFileMarker.utf8String)

    //replicated = replicated.dropRight(EndOfJournalFileMarker.length)
    assert(replicated.utf8String == controller0File.contentString)  // Compare strings for a legible error message
    assert(replicated == controller0File.byteArray)

    awaitAndAssert(observedLengths.lastOption.map(_.stripSuffix("\n").toLong) contains Files.size(controller0File))
    assert(observedLengths.last.stripSuffix("\n").toLong == Files.size(controller0File))

    // After a snapshot (a new journal file is started),
    // the web services returns the end of the currently read journal file and ends itself.
    httpControllerApi.executeCommand(ControllerCommand.TakeSnapshot).await(99.s)

    whenReplicated.await(9.s)
    awaitAndAssert(replicated endsWith EndOfJournalFileMarker)
    assert(replicated.utf8String == controller0File.contentString ++ EndOfJournalFileMarker.utf8String)  // Compare strings for legible error message
    assert(replicated == controller0File.byteArray ++ EndOfJournalFileMarker)

    whenLengthsObserved.await(9.s)
    assert(observedLengths.last == EndOfJournalFileMarker.utf8String)
    assert(observedLengths.dropRight(1).last.stripSuffix("\n").toLong == Files.size(controller0File))

  "Timeout" in:
    var eventId = controller.eventWatch.await[AgentDedicated](timeout = 9.s).last.eventId
    eventId = controller.eventWatch.await[AgentEventsObserved](timeout = 9.s, after = eventId).last.eventId
    val lines = httpClient.getRawLinesStream(Uri(s"$uri/controller/api/journal?timeout=0&markEOF=true&after=$eventId"))
      .await(99.s)
      .map(_.utf8String)
      .toListL
      .await(99.s)
    assert(lines.isEmpty)

  "Heartbeat" in:
    var lines = Vector.empty[String]
    var observedLines = Vector.empty[String]
    val fileAfter = controller.eventWatch.lastFileEventId
    val u = Uri(s"$uri/controller/api/journal?markEOF=true&file=$fileAfter&position=0")
    httpClient
      .getRawLinesStream(u, returnHeartbeatAs = Some(JournalEvent.StampedHeartbeatByteArray))
      .await(99.s)
      .foreach(o => IO:
        lines :+= o.utf8String)
      .compile.drain
      .unsafeRunAndForget()
    val observeWithHeartbeat = httpClient
      .getRawLinesStream(
        Uri(u.string + "&heartbeat=0.1"),
        returnHeartbeatAs = Some(JournalEvent.StampedHeartbeatByteArray))
      .await(99.s)
      .timeoutOnPull(2.s/*sometimes 1s is too short*/)  // Check heartbeat
      .onErrorEvalTap(t => IO(logger.error(t.toString)))
      .foreach(bytes => IO:
        observedLines :+= bytes.utf8String
        logger.debug(s"observeWithHeartbeat: ${bytes.utf8String.trim}"))
      .compile.drain
      .unsafeToFuture()

    val orderId = OrderId("🔷")
    httpControllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s)
    controller.eventWatch.await[OrderFinished](_.key == orderId)
    awaitAndAssert(lines.exists(_.contains("OrderFinished")))
    for o <- observeWithHeartbeat.value; t <- o.failed do throw t.appendCurrentStackTrace
    assert(lines.exists(_.contains("OrderFinished")))

    awaitAndAssert(observedLines.exists(_.contains("OrderFinished")))
    awaitAndAssert(observedLines.count(_ == JournalEvent.StampedHeartbeatString) > 1)
    awaitAndAssert(observedLines.filterNot(_ == JournalEvent.StampedHeartbeatString) == lines)


object JournalWebServiceTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT-111")
  private val pathExecutable = RelativePathExecutable("TEST.cmd")

  private val workflow = Workflow(WorkflowPath("test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentPath, pathExecutable))))
