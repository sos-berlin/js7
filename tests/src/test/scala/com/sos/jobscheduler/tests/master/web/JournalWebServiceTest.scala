package com.sos.jobscheduler.tests.master.web

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
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
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentReady
import com.sos.jobscheduler.tests.master.web.JournalWebServiceTest._
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import java.nio.file.Files
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scodec.bits.ByteVector

final class JournalWebServiceTest extends FreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = workflow :: Nil
  private lazy val uri = master.localUri
  private lazy val masterApi = AkkaHttpMasterApi(uri)(master.actorSystem).closeWithCloser

  override protected val masterConfig = ConfigFactory.parseString("""
    jobscheduler.auth.users {
      TEST-USER {
        password = "plain:TEST-PASSWORD",
      }
    }""")

  "/master/api/journal required authentication" in {
    val e = intercept[HttpException] {
      masterApi.getDecodedLinesObservable[String](s"$uri/master/api/journal?file=0&position=0") await 99.s
    }
    assert(e.status == Unauthorized)
  }

  "Login" in {
    masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "/master/api/journal" in {
    master.eventWatch.await[AgentReady](_ => true)  // Await last event

    var replicated = ByteVector.empty
    val whenReplicated = masterApi.getRawLinesObservable(s"$uri/master/api/journal?markEOF=true&file=0&position=0")
      .await(99.s)
      .foreach { replicated ++= _ }

    val observedLengths = mutable.Buffer[String]()
    val whenLengthsObserved = masterApi.getRawLinesObservable(s"$uri/master/api/journal?markEOF=true&file=0&position=0&return=length")
      .await(99.s)
      .foreach(o => observedLengths += o.utf8String)

    val stateDir = master.injector.instance[MasterConfiguration].stateDirectory
    val master0 = stateDir / "master--0.journal"
    waitForCondition(99.s, 10.ms)(replicated.length == Files.size(master0))
    //assert(replicated.utf8String endsWith EndOfJournalFileMarker.utf8String)

    //replicated = replicated.dropRight(EndOfJournalFileMarker.length)
    assert(replicated.utf8String == master0.contentString)  // Compare strings for a legible error message
    assert(replicated == master0.byteVector)

    waitForCondition(99.s, 10.ms)(observedLengths.lastOption.map(_.stripSuffix("\n").toLong) contains Files.size(master0))
    assert(observedLengths.last.stripSuffix("\n").toLong == Files.size(master0))

    // After a snapshot (a new journal file is started),
    // the web services returns the end of the currently read journal file and ends itself.
    masterApi.executeCommand(MasterCommand.TakeSnapshot) await 99.s
    assert(master0.contentString endsWith s"${JournalSeparators.EventFooter}\n")

    whenReplicated await 9.s
    waitForCondition(10.s, 10.ms)(replicated endsWith EndOfJournalFileMarker)
    assert(replicated.utf8String == master0.contentString ++ EndOfJournalFileMarker.utf8String)  // Compare strings for legible error message
    assert(replicated == master0.byteVector ++ EndOfJournalFileMarker)

    whenLengthsObserved await 9.s
    assert(observedLengths.last == EndOfJournalFileMarker.utf8String)
    assert(observedLengths.dropRight(1).last.stripSuffix("\n").toLong == Files.size(master0))
  }

  "Timeout" in {
    val lines = masterApi.getRawLinesObservable(s"$uri/master/api/journal?timeout=0&markEOF=true")
      .await(99.s)
      .map(_.utf8String)
      .toListL
      .await(99.s)
    assert(lines.isEmpty)
  }
}

object JournalWebServiceTest
{
  private val agentRefPath = AgentRefPath("/AGENT-111")
  private val TestExecutablePath = ExecutablePath("/TEST.cmd")

  private val workflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentRefPath, TestExecutablePath))))
}
