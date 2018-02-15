package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.scalautil.AutoClosing.{closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.tests.DirectoryProvider._
import io.circe.{Json, ObjectEncoder}
import java.nio.file.Files.createTempDirectory
import java.nio.file.{Files, Path}
import java.time.Duration
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private class DirectoryProvider(agentPaths: Seq[AgentPath]) extends HasCloser {

  val directory = createTempDirectory("test-") withCloser deleteDirectoryRecursively
  val master = new Tree(directory / "master")
  val agentToTree: Map[AgentPath, AgentTree] = agentPaths.map { o ⇒ o → new AgentTree(directory, o) }.toMap
  val agents = agentPaths.toVector map agentToTree
  closeOnError(this) {
    master.createDirectories()
     for (a ← agentToTree.values) {
      a.createDirectories()
      (master.live / s"${a.name}.agent.xml").xml = <agent uri={a.conf.localUri.toString}/>
    }
  }

  def run(body: (RunningMaster, IndexedSeq[RunningAgent]) ⇒ Unit)(implicit ec: ExecutionContext): Unit =
    runAgents(agents ⇒
      runMaster(master ⇒
        body(master, agents)))

  def runMaster(body: RunningMaster ⇒ Unit)(implicit ec: ExecutionContext): Unit =
    RunningMaster.runForTest(directory)(body)

  def runAgents(body: IndexedSeq[RunningAgent] ⇒ Unit)(implicit ec: ExecutionContext): Unit =
    multipleAutoClosing(agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents ⇒
      body(agents)
      agents map (_.terminate()) await 99.s
    }

  def agent(agentName: String) = new Tree(directory / agentName)
}

object DirectoryProvider {
  sealed class Tree(val directory: Path) {
    val config = directory / "config"
    val live = config / "live"
    val data = directory / "data"

    def createDirectories(): Unit = {
      Files.createDirectories(live)
      Files.createDirectory(data)
    }

    def writeJson[A: ObjectEncoder](path: TypedPath, a: A): Unit =
      file(path, SourceType.Json).contentString = Json.fromJsonObject(implicitly[ObjectEncoder[A]].encodeObject(a)).toPrettyString

    def writeTxt(path: TypedPath, content: String): Unit =
      file(path, SourceType.Txt).contentString = content

    def file(path: TypedPath, t: SourceType): Path =
      live resolve path.file(t)
  }

  final class AgentTree(rootDirectory: Path, val agentPath: AgentPath) extends Tree(rootDirectory / agentPath.name) {
    val name = agentPath.name
    lazy val conf = AgentConfiguration.forTest(Some(directory)).copy(name = name)
  }

  def jobXml(sleep: Duration = 0.s, variables: Map[String, String] = Map.empty, resultVariable: Option[String] = None) =
    <job tasks="3">
      <params>{
        for ((k, v) ← variables) yield <param name={k} value={v}/>
      }</params>
      <script language="shell">{script(sleep, resultVariable)}</script>
    </job>

  val StdoutOutput = if (isWindows) "TEST\r\n" else "TEST ☘\n"

  private def script(sleep: Duration, resultVariable: Option[String]) =
    if (isWindows)
      (s"""
        |@echo off
        |echo ${StdoutOutput.trim}
        |ping -n ${(sleep + 999999.µs).toSecondsString} 127.0.0.1 >nul""" +
        resultVariable.fold("")(o ⇒ s"""|echo result=SCRIPT-VARIABLE-%SCHEDULER_PARAM_${o.toUpperCase}% >>"%SCHEDULER_RETURN_VALUES%"""")
      ).stripMargin
    else
      (s"""
        |echo ${StdoutOutput.trim}
        |sleep ${sleep.toSecondsString}""" +
        resultVariable.fold("")(o ⇒ s"""|echo "result=SCRIPT-VARIABLE-$$SCHEDULER_PARAM_${o.toUpperCase}" >>"$$SCHEDULER_RETURN_VALUES"""")
      ).stripMargin
}
