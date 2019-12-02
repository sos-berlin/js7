package com.sos.jobscheduler.tests.master

import akka.util.Timeout
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.log.ScribeUtils
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.tests.master.MasterClusterTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.typesafe.config.ConfigFactory
import java.nio.file.Files
import java.nio.file.Files.readSymbolicLink
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.util.Try

/*
    Kommando AddClusterNode
    registiert einen neuen Cluster-Knoten, dessen Anmeldung akzeptiert werden wird.

    Ein Cluster-Knoten meldet sich an
    Abweisen, wenn nicht registriert

    Cluster-Knoten akzeptieren
    Cluster-Knoten übernimmt alle bisherigen Events
    Cluster-Knoten koppelt sich mit uns und wird passiv-gekoppelt.
    Wir werden aktiv-gekopppelt

    AKTIV-GEKOPPELT

    Jedes Event, das wir schreiben, lassen wir uns vom Passiven bestätigen

    Bei Störung (Passiver übernimmt Events nicht), die länger als eine Frist dauert:
    ? Passiver ist trotzdem erreichbar:
    ? => Passiver wird entkoppelt. Passiver bestätigt das. — Müssen wir den Fall berücksichtigen?
    ? => Aktiver wird aktiv-entkoppelt (also unsicher, falls erlaubt)

    Passiver ist nicht erreichbar.
    => Wir werden aktiv-gestört (Events sind gestoppt)
    => Agenten befragen, ob wir erreichbar sind (genauso wie der Passive fragt, ob der Aktive erreichbar ist)

    Absolute Mehrheit sagt, Aktiver ist nicht erreichbar:
    => Wir werden aktiv-entkoppelt

    Absolute Mehrheit sagt, Aktiver ist erreichbar:
    => Wir werden passiv-entkoppelt

    Keine absolute Mehrheit:
    => Wir werden passiv-entkoppelt — Zustand des Clusters ist unklar


    AKTIV-ENTKOPPELT


    PASSIV-GEKOPPELT

    Passiver übernimmt Events und bestätigt sie dem Aktiven

    Bei Störung (Aktiver ist nicht erreichbar), die länger als eine Frist dauert
    => Wir werden passiv-gestört
    => Agenten befragen, ob Aktiver erreichbar ist

    Absolute Mehrheit sagt, Aktiver ist nicht erreichbar:
    => Wir werden aktiv-entkoppelt

    Absolute Mehrheit sagt, Aktiver ist erreichbar:
    => Wir werden passiv-entkoppelt — Zustand des Clusters ist unklar
*/
final class MasterClusterTest extends FreeSpec
{
  ScribeUtils.coupleScribeWithSlf4j()

  private implicit val akkaTimeout = Timeout(88.s)

  "Cluster replicates journal files properly" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      val primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      primaryMaster.runOrder(FreshOrder(OrderId("🔶"), workflow.path))

      val backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      primaryMaster.eventWatch.await[ClusterEvent.FollowingStarted]()

      //assert(!primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
      //primaryMaster.executeCommandAsSystemUser(
      //  ClusterAppointBackup(Uri(backupMaster.localUri.toString), Uri(primaryMaster.localUri.toString))
      //).await(99.s).orThrow
      //primaryMaster.eventWatch.await[ClusterEvent.BackupNodeAppointed]()
      primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()
      assert(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)

      primaryMaster.runOrder(FreshOrder(OrderId("🔷"), workflow.path))

      def assertEqualJournalFiles(n: Int): Unit = {
        val journalFiles = listJournalFiles(primary.master.stateDir / "master")
        // Snapshot is not being acknowledged, so a new journal file starts asynchronously (or when one event has been written)
        waitForCondition(9.s, 10.ms) { listJournalFiles(backup.master.stateDir / "master").size == journalFiles.size }
        assert(journalFiles.size == n)
        for (primaryFile <- journalFiles.map(_.file)) {
          withClue(s"$primaryFile: ") {
            val backupJournalFile = backup.master.stateDir.resolve(primaryFile.getFileName)
            assert(backupJournalFile.contentString == primaryFile.contentString)
            assert(backupJournalFile.byteVector == primaryFile.byteVector)
          }
        }
        assert(readSymbolicLink(primary.master.stateDir / "master-journal") == journalFiles.last.file.getFileName)
      }

      assertEqualJournalFiles(1)

      primaryMaster.executeCommandAsSystemUser(MasterCommand.TakeSnapshot).await(99.s).orThrow
      assertEqualJournalFiles(2)

      primaryMaster.runOrder(FreshOrder(OrderId("🔵"), workflow.path))
      assertEqualJournalFiles(2)

      primaryMaster.terminate() await 99.s
      backupMaster.terminate() await 99.s
      assertEqualJournalFiles(3)

      // RESTART

      backup.runMaster(httpPort = Some(backupHttpPort)) { backupMaster =>
        primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
          assert(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          val lastEventId = primaryMaster.eventWatch.lastAddedEventId
          primaryMaster.runOrder(FreshOrder(OrderId("🔹"), workflow.path))
          primaryMaster.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)
          backupMaster.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)

          // Check acknowledgement of empty event list
          primaryMaster.httpApi.login(Some(UserId("TEST") -> SecretString("TEST-PASSWORD"))).await(99.s)
          primaryMaster.httpApi.addOrders(Nil).await(99.s)
        }
      }
    }
  }

  "Switchover" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      var lastEventId = EventId.BeforeFirst
      backup.runMaster(httpPort = Some(backupHttpPort)) { backupMaster =>
        primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
          //primaryMaster.executeCommandAsSystemUser(
          //  ClusterAppointBackup(activeUri = Uri(primaryMaster.localUri.toString), backupUri = Uri(backupMaster.localUri.toString))
          //).await(99.s).orThrow
          primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()
          val orderId = OrderId("⭕")
          primaryMaster.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO BACKUP
          primaryMaster.executeCommandAsSystemUser(MasterCommand.ClusterSwitchOver).await(99.s).orThrow
          primaryMaster.eventWatch.await[ClusterEvent.SwitchedOver]()
          for (t <- Try(primaryMaster.terminated.await(99.s)).failed) logger.error(s"Master terminated. $t")   // Erstmal beendet sich der Master nach SwitchOver

          backupMaster.eventWatch.await[ClusterEvent.SwitchedOver]()
          lastEventId = backupMaster.eventWatch.await[OrderFinished](_.key == orderId).head.eventId
        }

        assert(!backupMaster.journalActorState.isRequiringClusterAcknowledgement)

        // Start again the passive primary node
        primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
          backupMaster.eventWatch.await[ClusterEvent.ClusterCoupled](after = lastEventId)
          primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled](after = lastEventId)
          assert(backupMaster.journalActorState.isRequiringClusterAcknowledgement)

          val orderId = OrderId("🔴")
          backupMaster.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO PRIMARY
          backupMaster.executeCommandAsSystemUser(MasterCommand.ClusterSwitchOver).await(99.s).orThrow
          backupMaster.eventWatch.await[ClusterEvent.SwitchedOver]()
          Try(backupMaster.terminated.await(99.s)).failed foreach { t => logger.error(s"Master terminated. $t")}   // Erstmal beendet sich der Master nach SwitchOver
          assert(!primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          lastEventId = primaryMaster.eventWatch.await[OrderFinished](_.key == orderId).head.eventId

          locally {
            val orderId = OrderId("❌")
            primaryMaster.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
            primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          }
        }
      }
    }
  }

  "Backup node appointment is configurable in master.conf" in {
    pending   // Test not written, beacuse this feature is provided for internal manual tests only
  }

  private def withMasterAndBackup(primaryHttpPort: Int, backupHttpPort: Int)(body: (DirectoryProvider, DirectoryProvider) => Unit): Unit =
    withCloser { implicit closer =>
      val primary = new DirectoryProvider(agentRefPath :: Nil, workflow :: Nil, testName = Some("MasterClusterTest-Primary"),
        masterConfig = ConfigFactory.parseString(s"""
          jobscheduler.master.cluster.this-node.role = Primary
          jobscheduler.master.cluster.this-node.uri = "http://127.0.0.1:$primaryHttpPort"
          jobscheduler.master.cluster.other-node.uri = "http://127.0.0.1:$backupHttpPort"
          jobscheduler.auth.users.Master.password = "plain:BACKUP-MASTER-PASSWORD"
          jobscheduler.auth.users.TEST.password = "plain:TEST-PASSWORD"
          jobscheduler.auth.cluster.password = "PRIMARY-MASTER-PASSWORD" """)
      ).closeWithCloser

      val backup = new DirectoryProvider(Nil, Nil, testName = Some("MasterClusterTest-Backup"),
        masterConfig = ConfigFactory.parseString(s"""
          jobscheduler.master.cluster.this-node.role = Backup
          jobscheduler.master.cluster.this-node.uri = "http://127.0.0.1:$backupHttpPort"
          jobscheduler.master.cluster.other-node.uri = "http://127.0.0.1:$primaryHttpPort"
          jobscheduler.auth.users.Master.password = "plain:PRIMARY-MASTER-PASSWORD"
          jobscheduler.auth.cluster.password = "BACKUP-MASTER-PASSWORD" """)
      ).closeWithCloser

      // Replicate credentials required for agents
      Files.copy(primary.master.configDir / "private" / "private.conf", backup.master.configDir / "private" / "private.conf", REPLACE_EXISTING)

      primary.agents.head.writeExecutable(ExecutablePath("/TEST.cmd"), shellScript)

      primary.runAgents() { _ =>
        body(primary, backup)
      }
    }
}

object MasterClusterTest
{
  private val logger = Logger(getClass)
  private val agentRefPath = AgentRefPath("/AGENT")
  private val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW"),
    """define workflow {
      |  execute executable="/TEST.cmd", agent="/AGENT";
      |}""".stripMargin).orThrow

  private val shellScript = {
    val stdoutSize = 1*1000*1000
    val line = "." * 999
    (if (isWindows) "@echo off\n" else "") +
      (1 to stdoutSize / line.length)
        .map(i => "echo " + s"$i $line".take(line.length) + "\n")
        .mkString
  }
}
