package js7.tests.master.web

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.ScalaUtils.RichThrowableEither
import js7.base.utils.ScodecUtils._
import js7.base.web.Uri
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.data.agent.AgentRefPath
import js7.data.event.JournalSeparators
import js7.data.event.JournalSeparators.EndOfJournalFileMarker
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.client.AkkaHttpMasterApi
import js7.master.configuration.MasterConfiguration
import js7.master.data.MasterCommand
import js7.master.data.events.MasterAgentEvent.AgentReady
import js7.tests.master.web.JournalWebServiceTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import java.nio.file.Files
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scodec.bits.ByteVector

final class JournalWebServiceTest extends AnyFreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = workflow :: Nil
  private lazy val uri = master.localUri
  private lazy val masterApi = new AkkaHttpMasterApi(
    uri,
    Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))),
    master.actorSystem
  ).closeWithCloser
  private lazy val httpClient = masterApi.httpClient

  import masterApi.implicitSessionToken

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(executablePath, script(1.s))
  }

  override protected val masterConfig = ConfigFactory.parseString("""
    jobscheduler.auth.users {
      TEST-USER {
        password = "plain:TEST-PASSWORD",
      }
    }
    jobscheduler.journal.remove-obsolete-files = false""")

  "/master/api/journal requires authentication" in {
    val e = intercept[HttpException] {
      httpClient.getDecodedLinesObservable[String](Uri(s"$uri/master/api/journal?file=0&position=0")) await 99.s
    }
    assert(e.status == Unauthorized)
  }

  "Login" in {
    masterApi.login() await 99.s
  }

  "/master/api/journal" in {
    var replicated = ByteVector.empty
    master.eventWatch.await[AgentReady](_ => true)  // Await last event

    val whenReplicated = httpClient.getRawLinesObservable(Uri(s"$uri/master/api/journal?markEOF=true&file=0&position=0"))
      .await(99.s)
      .foreach { replicated ++= _ }

    val observedLengths = mutable.Buffer[String]()
    val whenLengthsObserved = httpClient.getRawLinesObservable(Uri(s"$uri/master/api/journal?markEOF=true&file=0&position=0&return=length"))
      .await(99.s)
      .foreach(o => observedLengths += o.utf8String)

    val stateDir = master.injector.instance[MasterConfiguration].stateDirectory
    val master0File = stateDir / "master--0.journal"
    waitForCondition(99.s, 10.ms)(replicated.length == Files.size(master0File))
    //assert(replicated.utf8String endsWith EndOfJournalFileMarker.utf8String)

    //replicated = replicated.dropRight(EndOfJournalFileMarker.length)
    assert(replicated.utf8String == master0File.contentString)  // Compare strings for a legible error message
    assert(replicated == master0File.byteVector)

    waitForCondition(99.s, 10.ms)(observedLengths.lastOption.map(_.stripSuffix("\n").toLong) contains Files.size(master0File))
    assert(observedLengths.last.stripSuffix("\n").toLong == Files.size(master0File))

    // After a snapshot (a new journal file is started),
    // the web services returns the end of the currently read journal file and ends itself.
    masterApi.executeCommand(MasterCommand.TakeSnapshot) await 99.s

    whenReplicated await 9.s
    waitForCondition(10.s, 10.ms)(replicated endsWith EndOfJournalFileMarker)
    assert(replicated.utf8String == master0File.contentString ++ EndOfJournalFileMarker.utf8String)  // Compare strings for legible error message
    assert(replicated == master0File.byteVector ++ EndOfJournalFileMarker)

    whenLengthsObserved await 9.s
    assert(observedLengths.last == EndOfJournalFileMarker.utf8String)
    assert(observedLengths.dropRight(1).last.stripSuffix("\n").toLong == Files.size(master0File))
  }

  "Timeout" in {
    val lines = httpClient.getRawLinesObservable(Uri(s"$uri/master/api/journal?timeout=0&markEOF=true"))
      .await(99.s)
      .map(_.utf8String)
      .toListL
      .await(99.s)
    assert(lines.isEmpty)
  }

  "Heartbeat" in {
    val lines = mutable.Buffer[String]()
    val heartbeatLines = mutable.Buffer[String]()
    val fileAfter = master.eventWatch.lastFileTornEventId
    val u = Uri(s"$uri/master/api/journal?markEOF=true&file=$fileAfter&position=0")
    httpClient.getRawLinesObservable(u).await(99.s)
      .foreach {
        lines += _.decodeUtf8.orThrow
      }
    httpClient.getRawLinesObservable(Uri(u.string + "&heartbeat=0.1")).await(99.s)
      .timeoutOnSlowUpstream(200.ms)  // Check heartbeat
      .foreach {
        heartbeatLines += _.decodeUtf8.orThrow
      }

    val orderId = OrderId("🔵")
    masterApi.addOrder(FreshOrder(orderId, workflow.path)) await 99.s
    master.eventWatch.await[OrderFinished](_.key == orderId)
    waitForCondition(1.s, 10.ms) { lines.exists(_ contains "OrderFinished") } shouldBe true
    waitForCondition(1.s, 10.ms) { heartbeatLines.exists(_ contains "OrderFinished") } shouldBe true

    assert(heartbeatLines.count(_ == JournalSeparators.HeartbeatMarker.utf8String) > 1)
    assert(heartbeatLines.filterNot(_ == JournalSeparators.HeartbeatMarker.utf8String) == lines)
  }
}

object JournalWebServiceTest
{
  private val agentRefPath = AgentRefPath("/AGENT-111")
  private val executablePath = ExecutablePath("/TEST.cmd")

  private val workflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentRefPath, executablePath))))
}
