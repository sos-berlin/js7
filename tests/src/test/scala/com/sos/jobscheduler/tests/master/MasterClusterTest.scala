package com.sos.jobscheduler.tests.master

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.tests.master.MasterClusterTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

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
  "test" in {
    //if (!sys.props.contains("MasterClusterTest")) pending
    withCloser { implicit closer =>
      val primary = new DirectoryProvider(
        agentRefPath :: Nil,
        workflow :: Nil,
        testName = Some("MasterClusterTest-Primary"),
        masterConfig = ConfigFactory.parseString("jobscheduler.webserver.auth.loopback-is-public = on"))
      primary.closeWithCloser
      val primaryAgents = primary.startAgents() await 99.s
      val primaryMaster = primary.startMaster() await 99.s
      primary.agents.head.writeExecutable(ExecutablePath("/TEST.cmd"), shellScript)

      def runOrder(orderId: OrderId) {
        primaryMaster.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        primaryMaster.eventWatch.await[OrderFinished](_.key == orderId)
      }
      runOrder(OrderId("🔵"))

      val backup = new DirectoryProvider(Nil, Nil, testName = Some("MasterClusterTest-Backup"),
        masterConfig = ConfigFactory.parseString(s"""
          jobscheduler.webserver.auth.loopback-is-public = on
          jobscheduler.master.cluster.primary-uri = "${primaryMaster.localUri}" """))
      val backupAgents = backup.startAgents() await 99.s
      val backupMaster = backup.startMaster() await 99.s
      val backupNodeId = ClusterNodeId((backup.master.stateDir / "ClusterNodeId").contentString)  // TODO Web service
      primaryMaster.executeCommandAsSystemUser(MasterCommand.AppointBackupNode(backupNodeId, Uri(backupMaster.localUri.toString)))
        .await(99.s).orThrow
      primaryMaster.eventWatch.await[ClusterEvent.FollowingStarted]()

      runOrder(OrderId("🔺"))

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
      }

      assertEqualJournalFiles(1)

      primaryMaster.executeCommandAsSystemUser(MasterCommand.TakeSnapshot).await(99.s).orThrow
      assertEqualJournalFiles(2)

      runOrder(OrderId("🔶"))
      assertEqualJournalFiles(2)

      primaryMaster.terminate() await 99.s
      primaryAgents.map(_.terminate()) await 99.s

      backupMaster.terminate() await 99.s
      backupAgents.map(_.terminate()) await 99.s

      assertEqualJournalFiles(3)
    }
  }
}

object MasterClusterTest
{
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
