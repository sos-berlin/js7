package com.sos.jobscheduler.tests.master.web

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.base.utils.ScodecUtils._
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.EndOfJournalFileMarker
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentReady
import com.sos.jobscheduler.tests.master.web.JournalWebServiceTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import java.nio.file.Files
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scodec.bits.ByteVector

final class JournalWebServiceTest extends FreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = workflow :: Nil
  private lazy val uri = master.localUri
  private lazy val masterApi = AkkaHttpMasterApi(uri)(master.actorSystem).closeWithCloser

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(executablePath, script(1.s))
  }

  override protected val masterConfig = ConfigFactory.parseString("""
    jobscheduler.auth.users {
      TEST-USER {
        password = "plain:TEST-PASSWORD",
      }
    }""")

  "/master/api/journal requires authentication" in {
    val e = intercept[HttpException] {
      masterApi.getDecodedLinesObservable[String](s"$uri/master/api/journal?file=0&position=0") await 99.s
    }
    assert(e.status == Unauthorized)
  }

  "Login" in {
    masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "/master/api/journal" in {
    var replicated = ByteVector.empty
    master.eventWatch.await[AgentReady](_ => true)  // Await last event

    val whenReplicated = masterApi.getRawLinesObservable(s"$uri/master/api/journal?markEOF=true&file=0&position=0")
      .await(99.s)
      .foreach { replicated ++= _ }

    val observedLengths = mutable.Buffer[String]()
    val whenLengthsObserved = masterApi.getRawLinesObservable(s"$uri/master/api/journal?markEOF=true&file=0&position=0&return=length")
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
    assert(master0File.contentString endsWith s"${JournalSeparators.EventFooter}\n")

    whenReplicated await 9.s
    waitForCondition(10.s, 10.ms)(replicated endsWith EndOfJournalFileMarker)
    assert(replicated.utf8String == master0File.contentString ++ EndOfJournalFileMarker.utf8String)  // Compare strings for legible error message
    assert(replicated == master0File.byteVector ++ EndOfJournalFileMarker)

    whenLengthsObserved await 9.s
    assert(observedLengths.last == EndOfJournalFileMarker.utf8String)
    assert(observedLengths.dropRight(1).last.stripSuffix("\n").toLong == Files.size(master0File))
  }

  "Timeout" in {
    val lines = masterApi.getRawLinesObservable(s"$uri/master/api/journal?timeout=0&markEOF=true")
      .await(99.s)
      .map(_.utf8String)
      .toListL
      .await(99.s)
    assert(lines.isEmpty)
  }

  "Heartbeat" in {
    var lines = mutable.Buffer[String]()
    var heartbeatLines = mutable.Buffer[String]()
    val fileAfter = master.eventWatch.lastFileTornEventId
    val u = s"$uri/master/api/journal?markEOF=true&file=$fileAfter&position=0"
    masterApi.getRawLinesObservable(u).await(99.s)
      .foreach {
        lines += _.decodeUtf8.orThrow
      }
    masterApi.getRawLinesObservable(u + "&heartbeat=0.1").await(99.s)
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
