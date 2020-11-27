package js7.tests

import cats.syntax.option._
import java.nio.file.Path
import js7.base.auth.UserAndPassword
import js7.base.convert.AsJava.StringAsPath
import js7.base.monixutils.MonixBase.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configutils.Configs._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.startup.JavaMain.runMain
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerCommand.UpdateRepo
import js7.data.agent.AgentName
import js7.data.item.{InventoryItem, InventoryItemSigner, VersionId}
import js7.data.job.ExecutablePath
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TestAddWorkflows._
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import scala.util.{Failure, Success, Try}

final class TestAddWorkflows(settings: Settings)
{
  def run(): Unit = {
    val directoryProvider = new DirectoryProvider(
      agentName :: Nil,
      controllerConfig = config"""
        js7.web.server.auth.public = true
        akka.stdout-loglevel = "OFF"""",
      useDirectory = settings.directory,
      controllerKeyStore = None)
    autoClosing(directoryProvider) { _ =>
      directoryProvider.run { (controller, _)  =>
        val commands = measureTime(settings.workflowCount, "workflows signed") {
          generateCommands(directoryProvider.itemSigner)
        }
        measureTime(settings.workflowCount, "workflows") {
          executeCommands(controller.localUri, commands)
        }
      }
    }
  }

  private def measureTime[A](n: Int, ops: String)(body: => A): A = {
    val stopwatch = new Stopwatch
    val a = body
    println(stopwatch.itemsPerSecondString(n, ops))
    a
  }

  private def generateCommands(signer: InventoryItemSigner[InventoryItem]): Seq[UpdateRepo] = {
    val workflow0 = Workflow.of(Execute(WorkflowJob(agentName, ExecutablePath("EXECUTABLE"))))
    val versionCounter = AtomicInt(0)
    Observable.fromIterable(1 to settings.workflowCount)
      .bufferTumbling(settings.bundleSize)
      .mapParallelUnordered(sys.runtime.availableProcessors)(is => Task {
        val v = VersionId(s"SPEED-${versionCounter.incrementAndGet()}")
        val workflows = for (i <- is) yield workflow0.withId(WorkflowPath(s"/WORKFLOW-$i") ~ v)
        UpdateRepo(v, workflows map signer.sign)
      }
    ).toL(Vector).await(99.s)
  }

  private def executeCommands(uri: Uri, commands: Seq[UpdateRepo]): Unit =
    AkkaHttpControllerApi.separateAkkaResource(uri, credentials)
      .use(controller =>
        controller.login() >>
          Observable
            .fromIterable(commands)
            .mapParallelUnordered(settings.parallelism)(
              controller.executeCommand(_))
            .completedL)
      .await(1.h)
}

object TestAddWorkflows
{
  private val agentName = AgentName("AGENT")
  private val credentials = none[UserAndPassword]

  def main(args: Array[String]): Unit =
    runMain {
      Try(Settings.fromArgs(args.toSeq)) match {
        case Failure(t) =>
          println(
            """usage: testAddWorkflows
              |  --workflow-count=10000
              |  --bundle-size=100
              |  --parallelism=2""".stripMargin)
          println(t.toString)
          System.exit(1)

        case Success(settings) =>
          new TestAddWorkflows(settings)
            .run()
      }
    }

  private final case class Settings(
    directory: Option[Path],
    workflowCount: Int,
    bundleSize: Int,
    parallelism: Int,
    useCluster: Boolean)

  private object Settings
  {
    private[TestAddWorkflows] def fromArgs(args: Seq[String]): Settings =
      CommandLineArguments.parse(args) { a =>
        Settings(
          directory = a.optionAs[Path]("--directory="),
          workflowCount = a.as[Int]("--workflow-count=", 10000),
          bundleSize = a.as[Int]("--bundle-size=", 100),
          parallelism = a.as[Int]("--parallelism=", 2),
          useCluster = false) //a.boolean("--cluster"))
      }
  }
}
