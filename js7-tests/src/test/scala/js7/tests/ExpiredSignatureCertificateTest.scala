package js7.tests

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.SignerId
import js7.base.crypt.x509.X509Algorithm.SHA512withRSA
import js7.base.crypt.x509.{Openssl, X509Cert, X509Signer}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isMac
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AlarmClock, TestAlarmClock, Timestamp, WallClock}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsBlocking.BlockingIOResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTestParallel
import js7.base.utils.{CatsBlocking, Missing}
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.{ItemSigner, VersionId}
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.Subagent
import js7.tests.ExpiredSignatureCertificateTest.*
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*

final class ExpiredSignatureCertificateTest extends OurTestSuite:

  private given IORuntime = ioRuntime
  private given Scheduler = ioRuntime.scheduler

  private val bareSubagentId = SubagentId("BARE")
  private lazy val bareSubagentItem =
    SubagentItem(bareSubagentId, agentPath, findFreeLocalUri())

  private val nextVersionId = Iterator.from(1).map(i => VersionId(i.toString))

  if isMac then
    // Requires macOS Homebrew openssl //
    "openssl -not_before= and -not_after=, distribute signer's certificate" - {
      lazy val now = ts"2050-10-01T12:00:00Z"
      given WallClock = WallClock.fixed(now)

      "All clocks are between notBefore and notAfter" - {
        def test(allowExpired: Boolean) =
          runMyTest(
            distributeCA = false,
            controllerTS = now, directorTS = now, bareSubagentTS = now,
            notBefore = now - 1.s, notAfter = now + 1.s,
            allowExpired = allowExpired): @unchecked

        "allow-expired-certificates = false" in:
          val Right(events) = test(allowExpired = false): @unchecked
          assert(events.contains(OrderFinished()))

        "allow-expired-certificates = true" in:
          val Right(events) = test(allowExpired = true): @unchecked
          assert(events.contains(OrderFinished()))
      }

      "Clock of Controller is after notAfter" - {
        def test(allowExpired: Boolean) =
          runMyTest(
            distributeCA = false,
            controllerTS = now + 2.s, directorTS = now, bareSubagentTS = now,
            notBefore = now - 1.s, notAfter = now + 1.s,
            allowExpired = allowExpired)

        "allow-expired-certificates = false" in:
          val checked = test(allowExpired = false): @unchecked
          assert(checked == Left(Problem:
            "java.security.cert.CertificateExpiredException: NotAfter: Sat Oct 01 14:00:01 CEST 2050"))

        "allow-expired-certificates = true" in:
          val checked = test(allowExpired = true): @unchecked
          assert(checked.isRight)
      }

      "Clock of Director is after notAfter" - {
        // TODO Timeout, because Controller doesn't check failed AgentCommand.AttachSignedItem
        //  nor AgentCommand.AttachOrder, and the order freezes in Attaching.
        "allow-expired-certificates = false" in:
          intercept[TimeoutException]:
            runMyTest(
              distributeCA = false,
              controllerTS = now, directorTS = now + 2.s, bareSubagentTS = now,
              notBefore = now - 1.s, notAfter = now + 1.s,
              timeout = 5.s,
              allowExpired = false)

        "allow-expired-certificates = true" in:
          val checked = runMyTest(
            distributeCA = false,
            controllerTS = now, directorTS = now + 2.s, bareSubagentTS = now,
            notBefore = now - 1.s, notAfter = now + 1.s,
            timeout = 5.s,
            allowExpired = true)
          assert(checked.isRight)
      }

      "Clock of bare Subagent is after notAfter" - {
        def test(allowExpired: Boolean) =
          runMyTest(
            distributeCA = false,
            controllerTS = now, directorTS = now, bareSubagentTS = now + 2.s,
            notBefore = now - 1.s, notAfter = now + 1.s,
            allowExpired = allowExpired): @unchecked

        "allow-expired-certificates = false" in:
          val Right(events) = test(allowExpired = false): @unchecked
          assert(events.contains(OrderProcessed(OrderOutcome.Disrupted:
            Problem("java.security.cert.CertificateExpiredException: NotAfter: Sat Oct 01 14:00:01 CEST 2050"))))

        "allow-expired-certificates = true" in:
          val Right(events) = test(allowExpired = true): @unchecked
          assert(events.contains(OrderFinished()))
      }
    }
  end if

  "openssl -days=1" - {
    "Distribute signer's certificate" - {
      addMyTests(distributeCA = false)
    }

    "Distribute a CA for multiple signers" - {
      addMyTests(distributeCA = true)
    }

    def addMyTests(distributeCA: Boolean) =
      "Not expired" - {
        val now = Timestamp.now + 1.minute
        given WallClock = WallClock.fixed(now)

        def test(allowExpired: Boolean) =
          runMyTest(
            distributeCA = distributeCA,
            controllerTS = now, directorTS = now, bareSubagentTS = now, days = 1,
            allowExpired = allowExpired): @unchecked

        "allow-expired-certificates = false" in:
          val Right(events) = test(allowExpired = false): @unchecked
          assert(events.contains(OrderFinished()))

        "allow-expired-certificates = true" in:
          val Right(events) = test(allowExpired = true): @unchecked
          assert(events.contains(OrderFinished()))
      }

      "Clock of Controller is after certificate's expiry" - {
        val now = Timestamp.now + 1.minute
        given WallClock = WallClock.fixed(now)

        def test(allowExpired: Boolean) =
          runMyTest(
            distributeCA = distributeCA,
            controllerTS = now + 2.days, directorTS = now, bareSubagentTS = now, days = 1,
            allowExpired = allowExpired)

        "allow-expired-certificates = false" in:
          val checked = test(allowExpired = false): @unchecked
          assert:
            checked.isLeft && checked.left.toOption.get.toString.startsWith:
              "java.security.cert.CertificateExpiredException: NotAfter: "

        "allow-expired-certificates = true" in:
          val checked = test(allowExpired = true): @unchecked
          assert(checked.isRight)
      }

      "Clock of Director is after certificate's expiry (Order stalls)" - {
        val now = Timestamp.now + 1.minute
        given WallClock = WallClock.fixed(now)

        "allow-expired-certificates = false" in:
          // TODO Timeout, because Controller doesn't check failed AgentCommand.AttachSignedItem
          //  nor AgentCommand.AttachOrder, and the order freezes in Attaching.
          intercept[TimeoutException]:
            runMyTest(
              distributeCA = distributeCA,
              controllerTS = now, directorTS = now + 2.days, bareSubagentTS = now, days = 1,
              timeout = if isTestParallel then 5.s else 2.s,
              allowExpired = false)

        "allow-expired-certificates = true" in:
          val checked = runMyTest(
            distributeCA = distributeCA,
            controllerTS = now, directorTS = now + 2.days, bareSubagentTS = now, days = 1,
            timeout = if isTestParallel then 5.s else 2.s,
            allowExpired = true)
      }

      "Clock of bare Subagent is after certificate's expiry" - {
        val now = Timestamp.now + 1.minute
        given WallClock = WallClock.fixed(now)

        def test(allowExpired: Boolean) =
          val Right(events) = runMyTest(
            distributeCA = distributeCA,
            controllerTS = now, directorTS = now, bareSubagentTS = now + 2.days, days = 1,
            allowExpired = allowExpired): @unchecked
          events.collectFirst { case OrderProcessed(o) => o }.get

        "allow-expired-certificates = false" in:
          val outcome = test(allowExpired = false): @unchecked
          assert:
            outcome.asInstanceOf[OrderOutcome.Disrupted].reason.problem.toString.startsWith:
              "java.security.cert.CertificateExpiredException: NotAfter: "

        "allow-expired-certificates = true" in:
          val outcome = test(allowExpired = true): @unchecked
          assert(outcome.isSucceeded)
      }
  }

  private def runMyTest(
    distributeCA: Boolean,
    controllerTS: Timestamp,
    directorTS: Timestamp,
    bareSubagentTS: Timestamp,
    days: Int | Missing = Missing,
    notBefore: Timestamp | Missing = Missing,
    notAfter: Timestamp | Missing = Missing,
    allowExpired: Boolean,
    timeout: FiniteDuration = 99.s)
    (using clock: WallClock)
  : Checked[Seq[OrderEvent]] =
    withTemporaryDirectory("ExpiredSignatureCertificateTest-"): workDir =>
      val controllerClock = TestAlarmClock(controllerTS)
      val directorClock = TestAlarmClock(directorTS)
      withDirectoryProvider(controllerClock, directorClock, allowExpired): directoryProvider =>
        directoryProvider.bareSubagentEnvResource(bareSubagentItem,
          director = directorsSubagentId,
          extraConfig = config"""
            js7.configuration.trusted-signature-keys.X509 =
              $${js7.config-directory}"/private/trusted-x509-keys"
            """
        ).blockingUse(timeout): bareSubagentEnv =>
          val openssl = Openssl(workDir)

          val (certWithPrivateKey, documentSigner) =
            if distributeCA then
              val root = openssl.Root("ROOT", days = days.notMissingOrThrow)
              val caCert = X509Cert.fromByteArray(
                root.certificateFile.byteArray,
                checkExpiry = Some(clock.now())
              ).orThrow
              assert(caCert.isCA)
              root.certWithPrivateKey -> root.Signer("SIGNER", days = days.notMissingOrThrow)
            else
              val certWithKey = openssl
                .generateCertWithPrivateKey("TEST", s"/$aSignerId",
                  days = days.toOption,
                  notBefore = notBefore.toOption,
                  notAfter = notAfter.toOption)
                .orThrow
              certWithKey ->
                X509Signer.checked(certWithKey.privateKey, SHA512withRSA, certWithKey.x509Cert)
                  .orThrow

          distributeCertificates(directoryProvider, certWithPrivateKey)

          val subagentTestWiring = Subagent.TestWiring(clock = TestAlarmClock(bareSubagentTS))
          bareSubagentEnv.subagentResource(subagentTestWiring).blockingUse(timeout): subagent =>
            directoryProvider.run: (controller, _) =>
              // Force the director to use the bare subagent
              controller.enableSubagents(directorsSubagentId -> false)

              val v = nextVersionId.next()
              val workflow = simpleWorkflow.withVersion(v)
              val itemSigner = ItemSigner(documentSigner, ControllerState.signableItemJsonCodec)
              val signedWorkflow = itemSigner.sign(workflow)
              logger.info(s"Signed workflow with ${documentSigner.toLongString}")
              for _ <- controller.api.updateRepo(v, Seq(signedWorkflow)).await(timeout) yield
                controller.runOrder(
                  FreshOrder(OrderId("ORDER"), workflow.path, deleteWhenTerminated = true),
                  timeout = timeout)
                .map(_.value)

  private def withDirectoryProvider[A](
    controllerClock: AlarmClock,
    directorClock: AlarmClock,
    allowExpired: Boolean)
    (body: DirectoryProvider => A)
  : A =
    val allowExpiredConfig = (allowExpired != ExpectedDefaultAllowExpired) ??
      s"js7.configuration.allow-expired-certificates = $allowExpired"

    autoClosing(
      DirectoryProvider(
        testName = Some("ExpiredSignatureCertificateTest"),
        agentPaths = Seq(agentPath),
        bareSubagentItems = Seq(bareSubagentItem),
        controllerConfig = config"""
          js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
          js7.configuration.trusted-signature-keys.X509 =
            $${js7.config-directory}"/private/trusted-x509-keys"
          $allowExpiredConfig
          """,
        agentConfig = config"""
          js7.job.execution.signed-script-injection-allowed = yes
          js7.configuration.trusted-signature-keys.X509 =
            $${js7.config-directory}"/private/trusted-x509-keys"
          $allowExpiredConfig
          """,
        controllerTestWiring = RunningController.TestWiring(alarmClock = Some(controllerClock)),
        subagentTestWiring = Subagent.TestWiring(clock = directorClock))
    ):
      body

  private def distributeCertificates(
    directoryProvider: DirectoryProvider,
    certWithKey: Openssl.CertWithPrivateKey)
  : Unit =
    val controllerKeyDir = directoryProvider.controllerEnv.trustedX509KeyDir
    val agentKeyDir = directoryProvider.agentEnvs.head.trustedX509KeyDir
    val bareSubagentKeyDir = directoryProvider
      .bareSubagentToDirectory(bareSubagentId) / "config" / "private" / "trusted-x509-keys"

    writeCertificate(controllerKeyDir, certWithKey.certificatePem)
    writeCertificate(agentKeyDir, certWithKey.certificatePem)
    writeCertificate(bareSubagentKeyDir, certWithKey.certificatePem)

  private def writeCertificate(dir: Path, pem: String): Unit =
    if !Files.exists(dir) then createDirectory(dir)
    dir / "test.pem" := pem


object ExpiredSignatureCertificateTest:

  private val logger = Logger[this.type]
  private val ExpectedDefaultAllowExpired = true
  private val agentPath = AgentPath("AGENT")
  private val directorsSubagentId = toLocalSubagentId(agentPath)
  private val aSignerId = SignerId("CN=SIGNER")

  private val simpleWorkflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
    Execute(WorkflowJob(agentPath, ShellScriptExecutable(":")))))
