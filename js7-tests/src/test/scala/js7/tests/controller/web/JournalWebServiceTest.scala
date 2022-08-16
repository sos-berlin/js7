package js7.tests.controller.web

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import java.nio.file.Files
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.Test
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.Closer.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.web.Uri
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.http.AkkaHttpClient.HttpException
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.{AgentDedicated, AgentEventsObserved, AgentReady}
import js7.data.controller.ControllerCommand
import js7.data.event.JournalSeparators
import js7.data.event.JournalSeparators.EndOfJournalFileMarker
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.web.JournalWebServiceTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable

final class JournalWebServiceTest extends Test with BeforeAndAfterAll with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(workflow)
  private lazy val uri = controller.localUri
  private lazy val httpControllerApi = new AkkaHttpControllerApi(
    uri,
    Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))),
    controller.actorSystem
  ).closeWithCloser
  private lazy val httpClient = httpControllerApi.httpClient

  import httpControllerApi.implicitSessionToken

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(pathExecutable, script(3.s))
  }

  override protected val controllerConfig = config"""
    js7.auth.users {
      TEST-USER {
        password = "plain:TEST-PASSWORD",
      }
    }
    js7.journal.remove-obsolete-files = false"""

  "/controller/api/journal requires authentication" in {
    val e = intercept[HttpException] {
      httpClient.getDecodedLinesObservable[String](Uri(s"$uri/controller/api/journal?file=0&position=0")) await 99.s
    }
    assert(e.status == Unauthorized)
  }

  "Login" in {
    httpControllerApi.login() await 99.s
  }

  "/controller/api/journal" in {
    pending // TODO Duplicate with JournalRouteTest, active node does not provide event acknowledgements

    var replicated = ByteArray.empty
    controller.eventWatch.await[AgentReady](_ => true)  // Await last event

    val whenReplicated = httpClient.getRawLinesObservable(Uri(s"$uri/controller/api/journal?markEOF=true&file=0&position=0"))
      .await(99.s)
      .foreach { replicated ++= _ }

    val observedLengths = mutable.Buffer[String]()
    val whenLengthsObserved = httpClient.getRawLinesObservable(Uri(s"$uri/controller/api/journal?markEOF=true&file=0&position=0&return=ack"))
      .await(99.s)
      .foreach(o => observedLengths += o.utf8String)

    val stateDir = controller.injector.instance[ControllerConfiguration].stateDirectory
    val controller0File = stateDir / "controller--0.journal"
    waitForCondition(99.s, 10.ms)(replicated.length == Files.size(controller0File))
    //assert(replicated.utf8String endsWith EndOfJournalFileMarker.utf8String)

    //replicated = replicated.dropRight(EndOfJournalFileMarker.length)
    assert(replicated.utf8String == controller0File.contentString)  // Compare strings for a legible error message
    assert(replicated == controller0File.byteArray)

    waitForCondition(99.s, 10.ms)(observedLengths.lastOption.map(_.stripSuffix("\n").toLong) contains Files.size(controller0File))
    assert(observedLengths.last.stripSuffix("\n").toLong == Files.size(controller0File))

    // After a snapshot (a new journal file is started),
    // the web services returns the end of the currently read journal file and ends itself.
    httpControllerApi.executeCommand(ControllerCommand.TakeSnapshot) await 99.s

    whenReplicated await 9.s
    waitForCondition(10.s, 10.ms)(replicated endsWith EndOfJournalFileMarker)
    assert(replicated.utf8String == controller0File.contentString ++ EndOfJournalFileMarker.utf8String)  // Compare strings for legible error message
    assert(replicated == controller0File.byteArray ++ EndOfJournalFileMarker)

    whenLengthsObserved await 9.s
    assert(observedLengths.last == EndOfJournalFileMarker.utf8String)
    assert(observedLengths.dropRight(1).last.stripSuffix("\n").toLong == Files.size(controller0File))
  }

  "Timeout" in {
    var eventId = controller.eventWatch.await[AgentDedicated](timeout = 9.s).last.eventId
    eventId = controller.eventWatch.await[AgentEventsObserved](timeout = 9.s, after = eventId).last.eventId
    val lines = httpClient.getRawLinesObservable(Uri(s"$uri/controller/api/journal?timeout=0&markEOF=true&after=$eventId"))
      .await(99.s)
      .map(_.utf8String)
      .toListL
      .await(99.s)
    assert(lines.isEmpty)
  }

  "Heartbeat" in {
    var lines = Vector.empty[String]
    var observedLines = Vector.empty[String]
    val fileAfter = controller.eventWatch.lastFileEventId
    val u = Uri(s"$uri/controller/api/journal?markEOF=true&file=$fileAfter&position=0")
    httpClient.getRawLinesObservable(u).await(99.s)
      .foreach {
        lines :+= _.utf8String
      }
    val observeWithHeartbeat = httpClient
      .getRawLinesObservable(Uri(u.string + "&heartbeat=0.1")).await(99.s)
      .timeoutOnSlowUpstream(2.s/*sometimes 1s is too short*/)  // Check heartbeat
      .doOnError(t => Task(scribe.error(t.toString)))
      .foreach { bytes =>
        observedLines :+= bytes.utf8String
        scribe.debug(s"observeWithHeartbeat: ${bytes.utf8String.trim}")
      }

    val orderId = OrderId("🔵")
    httpControllerApi.addOrder(FreshOrder(orderId, workflow.path)) await 99.s
    controller.eventWatch.await[OrderFinished](_.key == orderId)
    waitForCondition(9.s, 10.ms) { lines.exists(_ contains "OrderFinished") }
    for (o <- observeWithHeartbeat.value; t <- o.failed) throw t.appendCurrentStackTrace
    assert(lines.exists(_ contains "OrderFinished"))

    waitForCondition(9.s, 10.ms) { observedLines.exists(_ contains "OrderFinished") }
    assert(observedLines.exists(_ contains "OrderFinished"))

    assert(observedLines.count(_ == JournalSeparators.HeartbeatMarker.utf8String) > 1)
    assert(observedLines.filterNot(_ == JournalSeparators.HeartbeatMarker.utf8String) == lines)
  }
}

object JournalWebServiceTest
{
  private val agentPath = AgentPath("AGENT-111")
  private val pathExecutable = RelativePathExecutable("TEST.cmd")

  private val workflow = Workflow(WorkflowPath("test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentPath, pathExecutable))))
}
