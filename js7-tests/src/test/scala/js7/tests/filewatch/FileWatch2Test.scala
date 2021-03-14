package js7.tests.filewatch

import java.nio.file.Files.{createDirectories, createDirectory, exists}
import js7.agent.client.AgentClient
import js7.agent.data.event.AgentControllerEvent.AgentReadyForController
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.controller.ControllerEvent.ControllerShutDown
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest}
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemAttachable, SimpleItemAttached, SimpleItemAttachedToAgent, SimpleItemChanged}
import js7.data.item.{ItemRevision, SimpleItemEvent}
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderRemoved, OrderStarted, OrderStdoutWritten}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchId}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{OrderContext, OrderProcess, Result}
import js7.tests.filewatch.FileWatch2Test._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class FileWatch2Test extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentIds = Seq(aAgentId, bAgentId)
  protected val versionedItems = Seq(workflow)
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val aDirectory = directoryProvider.agents(0).dataDir / "tmp/a-files"
  private val bDirectory = directoryProvider.agents(0).dataDir / "tmp/b-files"

  val orderWatchId = OrderWatchId("TEST-SOURCE")
  private lazy val aFileWatch = FileWatch(
    orderWatchId,
    workflow.path,
    aAgentId,
    aDirectory.toString)
  private lazy val bFileWatch = aFileWatch.copy(
    directory = bDirectory.toString)

  private val orderId1 = OrderId("FileWatch:TEST-SOURCE:1")
  private val orderId2 = OrderId("FileWatch:TEST-SOURCE:2")
  private val orderId3 = OrderId("FileWatch:TEST-SOURCE:3")
  private val orderId4 = OrderId("FileWatch:TEST-SOURCE:4")
  private val orderId5 = OrderId("FileWatch:TEST-SOURCE:5")
  private val orderId6 = OrderId("FileWatch:TEST-SOURCE:6")
  private val orderId7 = OrderId("FileWatch:TEST-SOURCE:7")

  private def fileToOrderId(filename: String): OrderId =
    aFileWatch.generateOrderId(ExternalOrderName(filename)).orThrow

  "Start with some files" in {
    createDirectories(aDirectory)
    val initialFile = aDirectory / "1"
    initialFile := ""
    val initialOrderId = fileToOrderId("1")

    directoryProvider.runController() { controller =>
      controller.updateSimpleItemsAsSystemUser(Seq(aFileWatch)).await(99.s).orThrow
      // OrderWatch will be attached to the agent after next restart
    }

    directoryProvider.runController(dontWaitUntilReady = true) { controller =>
      import controller.eventWatch
      import controller.eventWatch.await

      directoryProvider.runAgents(Seq(bAgentId)) { _ =>
        directoryProvider.runAgents(Seq(aAgentId)) { _ =>
          await[SimpleItemAttached](_.event.id == orderWatchId)
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == initialOrderId)
          await[OrderRemoved](_.key == initialOrderId)
          assert(!exists(initialFile))

          aDirectory / "2" := ""
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == fileToOrderId("2"))
          await[OrderRemoved](_.key == fileToOrderId("2"))
          assert(!exists(aDirectory / "2"))

          assert(!semaphore.flatMap(_.tryAcquire).runSyncUnsafe())
          aDirectory / "3" := ""
          await[OrderStarted](_.key == fileToOrderId("3"))
        }

        // RESTART WATCHING AGENT WHILE A FILE EXISTS
        directoryProvider.runAgents(Seq(aAgentId)) { case Seq(aAgent) =>
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == fileToOrderId("3"))
          // Agent must detect the file deletion after restart to allow the order to be removed:
          await[OrderRemoved](_.key == fileToOrderId("3"))
          assert(!exists(aDirectory / "3"))

          // CHANGE DIRECTORY OF FILE ORDER SOURCE

          aDirectory / "4" := ""
          await[OrderStarted](_.key == fileToOrderId("4"))
          createDirectory(bDirectory)
          // The OrderWatch watches now the bDirectory,
          // but the running Order points to aDirectory.
          val beforeAttached = eventWatch.lastAddedEventId
          controller.updateSimpleItemsAsSystemUser(Seq(bFileWatch)).await(99.s).orThrow
          await[SimpleItemAttached](_.event.id == orderWatchId, after = beforeAttached)
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == fileToOrderId("4"))
          await[OrderRemoved](_.key == fileToOrderId("4"))
          assert(!exists(aDirectory / "4"))

          bDirectory / "5" := ""
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == fileToOrderId("5"))
          await[OrderRemoved](_.key == fileToOrderId("5"))
          assert(!exists(bDirectory / "5"))

          // DELETE AND RECREATE FILE WHILE ORDER IS RUNNING, YIELDING A SECOND ORDER
          bDirectory / "6" := ""
          await[OrderStarted](_.key == fileToOrderId("6"))
          semaphore.flatMap(_.releaseN(1)).runSyncUnsafe()
          await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("6"))
          bDirectory / "6" := ""
          await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"))
          semaphore.flatMap(_.releaseN(1)).runSyncUnsafe()
          val firstRemoved6 = await[OrderRemoved](_.key == fileToOrderId("6")).head.eventId

          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          assert(exists(bDirectory / "6"))
          await[OrderStarted](_.key == fileToOrderId("6"), after = firstRemoved6)
          await[OrderFinished](_.key == fileToOrderId("6"), after = firstRemoved6)
          await[OrderRemoved](_.key == fileToOrderId("6"), after = firstRemoved6)
          assert(!exists(bDirectory / "6"))

          // TODO DELETE DIRECTORY
          // The DirectoryWatch keeps observing the directory because only the name is deleted.
          // It must periodically reestablish the watch.
          //delete(bDirectory)
          //sleep(3.s)
          //createDirectory(bDirectory)
          //bDirectory / "7" := ""
          //await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("7"))
          //semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          //await[OrderFinished](_.key == fileToOrderId("7"))
          //await[OrderRemoved](_.key == fileToOrderId("7"))
          //assert(!exists(bDirectory / "7"))

          val client = AgentClient(agentUri = aAgent.localUri,
            directoryProvider.agents.head.userAndPassword)(aAgent.actorSystem)
          checkAgentEvents(client)
        }
      }

      checkControllerEvents(eventWatch.keyedEvents[Event](after = EventId.BeforeFirst))
    }

    // TODO Snapshot prüfen
    // TODO Verzeichnis löschen und wieder anlegen. FileWatchFailed event?
    // TODO frequent SimpleItemChanged
  }

  private def checkControllerEvents(keyedEvents: Seq[AnyKeyedEvent]): Unit = {
    val filteredLeyedEvents = keyedEvents
      .filter(_
        .event match {
          case _: ControllerShutDown => true
          case event: SimpleItemEvent if event.id.isInstanceOf[OrderWatchId] => true
          case _: OrderAdded => true
          case _: OrderStarted => true
          case _: OrderStdoutWritten => true
          case _: OrderFinished => true
          case _: OrderRemoved => true
          case _ => false
        })
    assert(filteredLeyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: SimpleItemAdded(aFileWatch),
      NoKey <-: SimpleItemAttachable(orderWatchId, aAgentId),
      NoKey <-: ControllerShutDown(None),
      NoKey <-: SimpleItemAttached(orderWatchId, aAgentId),
      orderId1 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/1")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("1")))),
      orderId1 <-: OrderStarted,
      orderId1 <-: OrderStdoutWritten(s"Deleted $aDirectory/1\n"),
      orderId1 <-: OrderFinished,
      orderId1 <-: OrderRemoved,

      orderId2 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/2")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("2")))),
      orderId2 <-: OrderStarted,
      orderId2 <-: OrderStdoutWritten(s"Deleted $aDirectory/2\n"),
      orderId2 <-: OrderFinished,
      orderId2 <-: OrderRemoved,

      orderId3 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/3")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("3")))),
      orderId3 <-: OrderStarted,
      orderId3 <-: OrderStdoutWritten(s"Deleted $aDirectory/3\n"),
      orderId3 <-: OrderFinished,
      orderId3 <-: OrderRemoved,

      orderId4 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/4")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("4")))),
      orderId4 <-: OrderStarted,
      NoKey <-: SimpleItemChanged(bFileWatch.copy(itemRevision = ItemRevision(1))),
      NoKey <-: SimpleItemAttachable(orderWatchId,aAgentId),
      NoKey <-: SimpleItemAttached(orderWatchId, aAgentId),
      orderId4 <-: OrderStdoutWritten(s"Deleted $aDirectory/4\n"),
      orderId4 <-: OrderFinished,
      orderId4 <-: OrderRemoved,

      orderId5 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/5")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("5")))),
      orderId5 <-: OrderStarted,
      orderId5 <-: OrderStdoutWritten(s"Deleted $bDirectory/5\n"),
      orderId5 <-: OrderFinished,
      orderId5 <-: OrderRemoved,

      orderId6 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/6")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStdoutWritten(s"Deleted $bDirectory/6\n"),
      orderId6 <-: OrderFinished,
      orderId6 <-: OrderRemoved,
      // And again
      orderId6 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/6")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStdoutWritten(s"Deleted $bDirectory/6\n"),
      orderId6 <-: OrderFinished,
      orderId6 <-: OrderRemoved))
  }

  private def checkAgentEvents(client: AgentClient): Unit = {
    client.login().await(99.s)
    val keyedEvents = client.controllersEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst))
      .await(99.s).orThrow.map(_.value)
      .flatMap(ke => Observable.fromIterable(Some(ke.event)
        .collect {
          case e: AgentReadyForController => e.copy(timezone = "UTC", totalRunningTime = 1.s)
          case e: SimpleItemAttachedToAgent => e
          case e: ExternalOrderArised => e
          case e: ExternalOrderVanished => e
        }
        .map(e => ke.copy(event = e))))
      .toListL.await(99.s)
    assert(keyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: AgentReadyForController("UTC", 1.s),
      NoKey <-: SimpleItemAttachedToAgent(aFileWatch),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("1"),
        Map("file" -> StringValue(s"$aDirectory/1"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("1")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("2"),
        Map("file" -> StringValue(s"$aDirectory/2"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("2")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("3"),
        Map("file" -> StringValue(s"$aDirectory/3"))),
      NoKey <-: AgentReadyForController("UTC", 1.s),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("3")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("4"),
        Map("file" -> StringValue(s"$aDirectory/4"))),
      NoKey <-: SimpleItemAttachedToAgent(bFileWatch.copy(itemRevision = ItemRevision(1))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("4")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("5"),
        Map("file" -> StringValue(s"$bDirectory/5"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("5")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("6"),
        Map("file" -> StringValue(s"$bDirectory/6"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("6")),
      // and again
      orderWatchId <-: ExternalOrderArised(ExternalOrderName("6"),
        Map("file" -> StringValue(s"$bDirectory/6"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("6"))))
  }
}

object FileWatch2Test
{
  private val aAgentId = AgentId("AGENT-A")
  private val bAgentId = AgentId("AGENT-B")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(bAgentId,
        InternalExecutable(classOf[SemaphoreJob].getName))),
      Execute(WorkflowJob(bAgentId,
        InternalExecutable(classOf[DeleteFileJob].getName))),
      Execute(WorkflowJob(bAgentId,
        InternalExecutable(classOf[SemaphoreJob].getName)))))

  private val semaphore = Semaphore[Task](0).memoize

  final class SemaphoreJob extends InternalJob
  {
    def processOrder(orderContext: OrderContext) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Right(Result(NamedValues.empty))))
  }
}
