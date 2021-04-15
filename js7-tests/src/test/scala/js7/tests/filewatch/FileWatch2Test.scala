package js7.tests.filewatch

import java.nio.file.Files.{createDirectories, createDirectory, delete, exists}
import js7.agent.client.AgentClient
import js7.agent.data.event.AgentControllerEvent.AgentReadyForController
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.controller.ControllerEvent.ControllerShutDown
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest}
import js7.data.item.CommonItemEvent.{ItemAttachable, ItemAttached, ItemAttachedToAgent}
import js7.data.item.SimpleItemEvent.{SimpleItemAdded, SimpleItemChanged}
import js7.data.item.{CommonItemEvent, InventoryItemEvent, ItemRevision, SimpleItemEvent}
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderRemoved, OrderStarted, OrderStderrWritten}
import js7.data.order.{OrderId, Outcome}
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchId}
import js7.data.value.StringValue
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
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
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  private val pollTimeout = if (isMac) "2.5s" else "1s"
  override protected def agentConfig = config"""
    js7.filewatch.poll-timeout = $pollTimeout
    js7.filewatch.watch-delay = 1ms
    js7.journal.remove-obsolete-files = false
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val aDirectory = directoryProvider.agents(0).dataDir / "tmp/a-files"
  private val bDirectory = directoryProvider.agents(0).dataDir / "tmp/b-files"

  val orderWatchId = OrderWatchId("FILE-WATCH")

  private lazy val aFileWatch = FileWatch(
    orderWatchId,
    workflow.path,
    aAgentId,
    aDirectory.toString)
  private lazy val bFileWatch = aFileWatch.copy(directory = bDirectory.toString)

  private val orderId1 = OrderId("file:FILE-WATCH:1")
  private val orderId2 = OrderId("file:FILE-WATCH:2")
  private val orderId3 = OrderId("file:FILE-WATCH:3")
  private val orderId4 = OrderId("file:FILE-WATCH:4")
  private val orderId5 = OrderId("file:FILE-WATCH:5")
  private val orderId6 = OrderId("file:FILE-WATCH:6")
  private val orderId7 = OrderId("file:FILE-WATCH:7")

  "A sequence of directory changes" in {
    createDirectories(aDirectory)
    val initialFile = aDirectory / "1"
    initialFile := ""
    val initialOrderId = orderId1

    directoryProvider.runController() { controller =>
      controller.updateSimpleItemsAsSystemUser(Seq(aFileWatch)).await(99.s).orThrow
      // OrderWatch will be attached to the agent after next restart
    }

    directoryProvider.runController(dontWaitUntilReady = true) { controller =>
      import controller.eventWatch
      import controller.eventWatch.await

      directoryProvider.runAgents(Seq(bAgentId)) { _ =>
        directoryProvider.runAgents(Seq(aAgentId)) { _ =>
          await[ItemAttached](_.event.id == orderWatchId)
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == initialOrderId)
          await[OrderRemoved](_.key == initialOrderId)
          assert(!exists(initialFile))

          locally {
            val orderId = orderId2
            val file = aDirectory / "2"
            file := ""
            semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
            await[OrderFinished](_.key == orderId)
            await[OrderRemoved](_.key == orderId)
            assert(!exists(file))
          }

          assert(!semaphore.flatMap(_.tryAcquire).runSyncUnsafe())
          aDirectory / "3" := ""
          await[OrderStarted](_.key == orderId3)
        }

        // RESTART WATCHING AGENT WHILE A FILE EXISTS
        directoryProvider.runAgents(Seq(aAgentId)) { case Seq(aAgent) =>
          semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
          await[OrderFinished](_.key == orderId3)
          // Agent must detect the file deletion after restart to allow the order to be removed:
          await[OrderRemoved](_.key == orderId3)
          assert(!exists(aDirectory / "3"))

          locally {
            // CHANGE DIRECTORY OF FILE ORDER SOURCE
            val orderId = orderId4
            val file = aDirectory / "4"
            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("4"))
            await[OrderStarted](_.key == orderId)
            createDirectory(bDirectory)
            val beforeAttached = eventWatch.lastAddedEventId
            controller.updateSimpleItemsAsSystemUser(Seq(bFileWatch)).await(99.s).orThrow
            await[ItemAttached](ke => ke.event.id == orderWatchId && ke.event.itemRevision == Some(ItemRevision(1)),
              after = beforeAttached)
            // The OrderWatch watches now the bDirectory, but the running Order points to aDirectory.
            // bDirectory does not contain the file
            await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("4"))
            semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
            await[OrderFinished](_.key == orderId)
            await[OrderRemoved](_.key == orderId)
            assert(!exists(file))
          }

          locally {
            val orderId = orderId5
            val file = bDirectory / "5"
            file := ""
            semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
            await[OrderFinished](_.key == orderId)
            await[OrderRemoved](_.key == orderId)
            assert(!exists(file))
          }

          locally {
            // DELETE AND RECREATE FILE WHILE ORDER IS RUNNING, YIELDING A SECOND ORDER
            val orderId = orderId6
            val file = bDirectory / "6"
            file := ""
            await[OrderStarted](_.key == orderId)
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"))

            semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
            val vanished = await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("6"))
              .head.eventId

            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"), after = vanished)

            semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
            val firstRemoved = await[OrderRemoved](_.key == orderId).head.eventId

            await[OrderStarted](_.key == orderId, after = firstRemoved)
            await[OrderFinished](_.key == orderId, after = firstRemoved)
            await[OrderRemoved](_.key == orderId, after = firstRemoved)
            assert(!exists(file))
          }

          locally {
            // DELETE DIRECTORY
            // The DirectoryWatch keeps observing the directory because only the name is deleted.
            // So it periodically rereads the directory and restarts the watch.
            val orderId = orderId7
            val file = bDirectory / "7"
            delete(bDirectory)
            sleep(500.ms)
            createDirectory(bDirectory)
            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("7"))
            semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
            await[OrderFinished](_.key == orderId)
            await[OrderRemoved](_.key == orderId)
            assert(!exists(file))
          }
          val client = AgentClient(agentUri = aAgent.localUri,
            directoryProvider.agents.head.userAndPassword)(aAgent.actorSystem)
          checkAgentEvents(client)
        }
      }

      checkControllerEvents(eventWatch.keyedEvents[Event](after = EventId.BeforeFirst))
    }
  }

  private def checkControllerEvents(keyedEvents: Seq[AnyKeyedEvent]): Unit = {
    val filteredLeyedEvents = keyedEvents
      .filter(_
        .event match {
          case _: ControllerShutDown => true
          case _: CommonItemEvent => true
          case event: SimpleItemEvent if event.id.isInstanceOf[OrderWatchId] => true
          case _: OrderAdded => true
          case _: OrderStarted => true
          case _: OrderStderrWritten => true
          case _: OrderFinished => true
          case _: OrderRemoved => true
          case _ => false
        })
    assert(filteredLeyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: SimpleItemAdded(aFileWatch.copy(itemRevision = Some(ItemRevision(0)))),
      NoKey <-: ItemAttachable(orderWatchId, aAgentId),
      NoKey <-: ControllerShutDown(None),
      NoKey <-: ItemAttached(orderWatchId, Some(ItemRevision(0)), aAgentId),
      orderId1 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/1")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("1")))),
      NoKey <-: ItemAttached(workflow.id, None, bAgentId),
      orderId1 <-: OrderStarted,
      orderId1 <-: OrderStderrWritten(s"Deleted $aDirectory/1\n"),
      orderId1 <-: OrderFinished,
      orderId1 <-: OrderRemoved,

      orderId2 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/2")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("2")))),
      orderId2 <-: OrderStarted,
      orderId2 <-: OrderStderrWritten(s"Deleted $aDirectory/2\n"),
      orderId2 <-: OrderFinished,
      orderId2 <-: OrderRemoved,

      orderId3 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/3")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("3")))),
      orderId3 <-: OrderStarted,
      orderId3 <-: OrderStderrWritten(s"Deleted $aDirectory/3\n"),
      orderId3 <-: OrderFinished,
      orderId3 <-: OrderRemoved,

      orderId4 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$aDirectory/4")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("4")))),
      orderId4 <-: OrderStarted,
      NoKey <-: SimpleItemChanged(bFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
      NoKey <-: ItemAttachable(orderWatchId, aAgentId),
      NoKey <-: ItemAttached(orderWatchId, Some(ItemRevision(1)), aAgentId),
      orderId4 <-: OrderStderrWritten(s"Deleted $aDirectory/4\n"),
      orderId4 <-: OrderFinished,
      orderId4 <-: OrderRemoved,

      orderId5 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/5")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("5")))),
      orderId5 <-: OrderStarted,
      orderId5 <-: OrderStderrWritten(s"Deleted $bDirectory/5\n"),
      orderId5 <-: OrderFinished,
      orderId5 <-: OrderRemoved,

      orderId6 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/6")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStderrWritten(s"Deleted $bDirectory/6\n"),
      orderId6 <-: OrderFinished,
      orderId6 <-: OrderRemoved,
      // And again
      orderId6 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/6")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStderrWritten(s"Deleted $bDirectory/6\n"),
      orderId6 <-: OrderFinished,
      orderId6 <-: OrderRemoved,

      orderId7 <-: OrderAdded(workflow.id,
        Map(FileArgumentName -> StringValue(s"$bDirectory/7")),
        None,
        Some(ExternalOrderKey(orderWatchId, ExternalOrderName("7")))),
      orderId7 <-: OrderStarted,
      orderId7 <-: OrderStderrWritten(s"Deleted $bDirectory/7\n"),
      orderId7 <-: OrderFinished,
      orderId7 <-: OrderRemoved))
  }

  private def checkAgentEvents(client: AgentClient): Unit = {
    client.login().await(99.s)
    val keyedEvents = client.controllersEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst))
      .await(99.s).orThrow.map(_.value)
      .flatMap(ke => Observable.fromIterable(Some(ke.event)
        .collect {
          case e: AgentReadyForController => e.copy(timezone = "UTC", totalRunningTime = 1.s)
          case e: InventoryItemEvent => e
          case e: OrderWatchEvent => e
        }
        .map(e => ke.copy(event = e))))
      .toListL.await(99.s)
    assert(keyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: AgentReadyForController("UTC", 1.s),
      NoKey <-: ItemAttachedToAgent(aFileWatch.copy(itemRevision = Some(ItemRevision(0)))),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("1"),
        OrderId("file:FILE-WATCH:1"),
        Map("file" -> StringValue(s"$aDirectory/1"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("1")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("2"),
        OrderId("file:FILE-WATCH:2"),
        Map("file" -> StringValue(s"$aDirectory/2"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("2")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("3"),
        OrderId("file:FILE-WATCH:3"),
        Map("file" -> StringValue(s"$aDirectory/3"))),
      NoKey <-: AgentReadyForController("UTC", 1.s),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("3")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("4"),
        OrderId("file:FILE-WATCH:4"),
        Map("file" -> StringValue(s"$aDirectory/4"))),
      NoKey <-: ItemAttachedToAgent(bFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("4")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("5"),
        OrderId("file:FILE-WATCH:5"),
        Map("file" -> StringValue(s"$bDirectory/5"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("5")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("6"),
        OrderId("file:FILE-WATCH:6"),
        Map("file" -> StringValue(s"$bDirectory/6"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("6")),
      // and again
      orderWatchId <-: ExternalOrderArised(ExternalOrderName("6"),
        OrderId("file:FILE-WATCH:6"),
        Map("file" -> StringValue(s"$bDirectory/6"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("6")),

      orderWatchId <-: ExternalOrderArised(ExternalOrderName("7"),
        OrderId("file:FILE-WATCH:7"),
        Map("file" -> StringValue(s"$bDirectory/7"))),
      orderWatchId <-: ExternalOrderVanished(ExternalOrderName("7"))))
  }
}

object FileWatch2Test
{
  private val aAgentId = AgentId("AGENT-A")
  private val bAgentId = AgentId("AGENT-B")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(bAgentId, InternalExecutable(classOf[SemaphoreJob].getName))),
      Execute(WorkflowJob(bAgentId, InternalExecutable(classOf[DeleteFileJob].getName))),
      Execute(WorkflowJob(bAgentId, InternalExecutable(classOf[SemaphoreJob].getName)))))

  private val semaphore = Semaphore[Task](0).memoize

  final class SemaphoreJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Outcome.succeeded))
  }
}
