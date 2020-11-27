package js7.tests.controller.web

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import java.nio.file.Files
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.generic.SecretString
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.web.Uri
import js7.common.configutils.Configs._
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.ControllerCommand
import js7.controller.data.events.AgentRefStateEvent.{AgentReady, AgentRegisteredController}
import js7.data.agent.AgentName
import js7.data.event.JournalSeparators
import js7.data.event.JournalSeparators.EndOfJournalFileMarker
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.web.JournalWebServiceTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable

final class JournalWebServiceTest extends AnyFreeSpec with BeforeAndAfterAll with ControllerAgentForScalaTest
{
  protected val agentNames = agentName :: Nil
  protected val inventoryItems = workflow :: Nil
  private lazy val uri = controller.localUri
  private lazy val controllerApi = new AkkaHttpControllerApi(
    uri,
    Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))),
    controller.actorSystem
  ).closeWithCloser
  private lazy val httpClient = controllerApi.httpClient

  import controllerApi.implicitSessionToken

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(executablePath, script(1.s))
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
    controllerApi.login() await 99.s
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
    controllerApi.executeCommand(ControllerCommand.TakeSnapshot) await 99.s

    whenReplicated await 9.s
    waitForCondition(10.s, 10.ms)(replicated endsWith EndOfJournalFileMarker)
    assert(replicated.utf8String == controller0File.contentString ++ EndOfJournalFileMarker.utf8String)  // Compare strings for legible error message
    assert(replicated == controller0File.byteArray ++ EndOfJournalFileMarker)

    whenLengthsObserved await 9.s
    assert(observedLengths.last == EndOfJournalFileMarker.utf8String)
    assert(observedLengths.dropRight(1).last.stripSuffix("\n").toLong == Files.size(controller0File))
  }

  "Timeout" in {
    val eventId = controller.eventWatch.await[AgentRegisteredController](timeout = 9.s).last.eventId
    val lines = httpClient.getRawLinesObservable(Uri(s"$uri/controller/api/journal?timeout=0&markEOF=true&after=$eventId"))
      .await(99.s)
      .map(_.utf8String)
      .toListL
      .await(99.s)
    assert(lines.isEmpty)
  }

  "Heartbeat" in {
    val lines = mutable.Buffer[String]()
    val heartbeatLines = mutable.Buffer[String]()
    val fileAfter = controller.eventWatch.lastFileTornEventId
    val u = Uri(s"$uri/controller/api/journal?markEOF=true&file=$fileAfter&position=0")
    httpClient.getRawLinesObservable(u).await(99.s)
      .foreach {
        lines += _.utf8String
      }
    httpClient.getRawLinesObservable(Uri(u.string + "&heartbeat=0.1")).await(99.s)
      .timeoutOnSlowUpstream(200.ms)  // Check heartbeat
      .foreach {
        heartbeatLines += _.utf8String
      }

    val orderId = OrderId("🔵")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path)) await 99.s
    controller.eventWatch.await[OrderFinished](_.key == orderId)
    waitForCondition(9.s, 10.ms) { lines.exists(_ contains "OrderFinished") } shouldBe true
    waitForCondition(9.s, 10.ms) { heartbeatLines.exists(_ contains "OrderFinished") } shouldBe true

    assert(heartbeatLines.count(_ == JournalSeparators.HeartbeatMarker.utf8String) > 1)
    assert(heartbeatLines.filterNot(_ == JournalSeparators.HeartbeatMarker.utf8String) == lines)
  }
}

object JournalWebServiceTest
{
  private val agentName = AgentName("AGENT-111")
  private val executablePath = RelativeExecutablePath("TEST.cmd")

  private val workflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentName, executablePath))))
}
