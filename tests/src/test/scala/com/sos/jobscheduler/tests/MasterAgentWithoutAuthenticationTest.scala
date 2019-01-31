package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.core.signature.PgpCommons.writePublicKeyAsAscii
import com.sos.jobscheduler.core.signature.{PgpKeyGenerator, PgpUserId}
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand.ReplaceRepo
import com.sos.jobscheduler.master.data.MasterFileBaseds
import com.sos.jobscheduler.tests.MasterAgentWithoutAuthenticationTest._
import java.io.FileOutputStream
import java.nio.file.Files.createDirectories
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentWithoutAuthenticationTest extends FreeSpec
{
  protected def agentPaths = agentPath :: Nil
  protected def fileBased = workflow :: Nil

  "test" in {
    withTemporaryDirectory("MasterAgentWithoutAuthenticationTest-") { dir ⇒
      createDirectories(dir / "master/config/private")
      createDirectories(dir / "master/data/state")
      createDirectories(dir / "agent/config/executables")
      createDirectories(dir / "agent/data/state")

      dir / "agent/config/agent.conf" :=
        """jobscheduler.webserver.auth.public = true
          |""".stripMargin

      (dir / "agent/config/executables/EXECUTABLE.cmd").writeExecutable(":")

      val masterPort :: agentPort :: Nil = FreeTcpPortFinder.findRandomFreeTcpPorts(2)
      val agentConfiguration = AgentConfiguration.fromCommandLine(
        "-config-directory=" + dir / "agent/config" ::
        "-data-directory=" + dir / "agent/data" ::
        "-http-port=" + agentPort :: Nil)

      val agentRef = Agent(agentPath % versionId, s"http://127.0.0.1:$agentPort")
      val agentFuture = RunningAgent(agentConfiguration)

      val masterConfiguration = MasterConfiguration.fromCommandLine(
        "-config-directory=" + dir / "master/config" ::
        "-data-directory=" + dir / "master/data" ::
        "-http-port=" + masterPort :: Nil)

      val pgpSecretKeyPassword = SecretString("PGP-PASSWORD")
      val pgpSecretKey = PgpKeyGenerator.generateSecretKey(
        PgpUserId("MasterAgentWithoutAuthenticationTest"),
        pgpSecretKeyPassword,
        keySize = 1024)
      val fileBasedSigner = new FileBasedSigner(MasterFileBaseds.jsonCodec, pgpSecretKey, pgpSecretKeyPassword)
      autoClosing(new FileOutputStream(dir / "master/config/private/trusted-pgp-keys.asc")) { out ⇒
        writePublicKeyAsAscii(pgpSecretKey.getPublicKey, out)
      }

      val agent = agentFuture await 99.seconds
      val master = RunningMaster(masterConfiguration) await 99.seconds

      val replaceRepo = ReplaceRepo(versionId, (agentRef :: workflow :: Nil) map fileBasedSigner.sign)
      master.executeCommandAsSystemUser(replaceRepo).runSyncUnsafe(99.seconds).orThrow
      master.addOrder(FreshOrder(orderId, workflow.path)).runSyncUnsafe(99.seconds).orThrow
      master.eventWatch.await[OrderFinished](_.key == orderId)

      master.terminate().runSyncUnsafe(99.seconds)
      agent.terminate().runSyncUnsafe(99.seconds)
    }
  }
}

object MasterAgentWithoutAuthenticationTest
{
  private val versionId = VersionId("INITIAL")
  private val agentPath = AgentPath("/AGENT")
  private val executablePath = ExecutablePath("/EXECUTABLE.cmd")
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW") % versionId,
    Execute(WorkflowJob(agentPath, executablePath)))
  private val orderId = OrderId("🔵")
}
