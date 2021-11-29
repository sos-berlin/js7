package js7.tests.filewatch

import java.nio.file.Files.{createDirectories, createDirectory, delete, exists}
import js7.agent.client.AgentClient
import js7.agent.data.event.AgentEvent.AgentReady
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.controller.ControllerEvent.ControllerShutDown
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedStateEvent, ItemAttachedToMe}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItemEvent, ItemRevision, UnsignedSimpleItemEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderStarted, OrderStderrWritten}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.subagent.SubagentId
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{MkString, StringConstant}
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatch2Test._
import js7.tests.jobs.{DeleteFileJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class FileWatch2Test extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentPaths = Seq(aAgentPath, bAgentPath)
  protected val items = Seq(workflow)
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

  private val aDirectory = directoryProvider.agents(0).dataDir / "work/a-files"
  private val bDirectory = directoryProvider.agents(0).dataDir / "work/b-files"

  val orderWatchPath = OrderWatchPath("FILE-WATCH")

  private lazy val aFileWatch = FileWatch(
    orderWatchPath,
    workflow.path,
    aAgentPath,
    StringConstant(aDirectory.toString))
  private lazy val bFileWatch = aFileWatch.copy(directory = StringConstant(bDirectory.toString))

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
      controller.updateUnsignedSimpleItemsAsSystemUser(Seq(aFileWatch)).await(99.s).orThrow
      // OrderWatch will be attached to the agent after next restart
    }

    directoryProvider.runController(dontWaitUntilReady = true) { controller =>
      import controller.eventWatch
      import controller.eventWatch.await

      directoryProvider.runAgents(Seq(bAgentPath)) { _ =>
        directoryProvider.runAgents(Seq(aAgentPath)) { _ =>
          await[ItemAttached](_.event.key == orderWatchPath)
          TestJob.continue(2)
          await[OrderFinished](_.key == initialOrderId)
          await[OrderDeleted](_.key == initialOrderId)
          assert(!exists(initialFile))

          locally {
            val orderId = orderId2
            val file = aDirectory / "2"
            file := ""
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))
          }

          assert(!TestJob.semaphore.flatMap(_.tryAcquire).runSyncUnsafe())
          aDirectory / "3" := ""
          await[OrderStarted](_.key == orderId3)
        }

        // RESTART WATCHING AGENT WHILE A FILE EXISTS
        directoryProvider.runAgents(Seq(aAgentPath)) { case Seq(aAgent) =>
          TestJob.continue(2)
          await[OrderFinished](_.key == orderId3)
          // Agent must detect the file deletion after restart to allow the order to be removed:
          await[OrderDeleted](_.key == orderId3)
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
            controller.updateUnsignedSimpleItemsAsSystemUser(Seq(bFileWatch)).await(99.s).orThrow
            await[ItemAttached](ke => ke.event.key == orderWatchPath && ke.event.itemRevision == Some(ItemRevision(1)),
              after = beforeAttached)
            // The OrderWatch watches now the bDirectory, but the running Order points to aDirectory.
            // bDirectory does not contain the file
            await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("4"))
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))
          }

          locally {
            val orderId = orderId5
            val file = bDirectory / "5"
            file := ""
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))
          }

          locally {
            // DELETE AND RECREATE FILE WHILE ORDER IS RUNNING, YIELDING A SECOND ORDER
            val orderId = orderId6
            val file = bDirectory / "6"
            file := ""
            await[OrderStarted](_.key == orderId)
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"))

            TestJob.continue(2)
            val vanished = await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("6"))
              .head.eventId

            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"), after = vanished)

            TestJob.continue(2)
            val firstRemoved = await[OrderDeleted](_.key == orderId).head.eventId

            await[OrderStarted](_.key == orderId, after = firstRemoved)
            await[OrderFinished](_.key == orderId, after = firstRemoved)
            await[OrderDeleted](_.key == orderId, after = firstRemoved)
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
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
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
          case e: ItemAttachedStateEvent if e.key.isInstanceOf[SubagentId] => false
          case _: BasicItemEvent => true
          case e: UnsignedSimpleItemEvent if e.key.isInstanceOf[OrderWatchPath] => true
          case _: OrderAdded => true
          case _: OrderStarted => true
          case _: OrderStderrWritten => true
          case _: OrderFinished => true
          case _: OrderDeleted => true
          case _ => false
        })
    assert(filteredLeyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: UnsignedSimpleItemAdded(aFileWatch.copy(itemRevision = Some(ItemRevision(0)))),
      NoKey <-: ItemAttachable(orderWatchPath, aAgentPath),
      NoKey <-: ControllerShutDown(None),
      NoKey <-: ItemAttached(orderWatchPath, Some(ItemRevision(0)), aAgentPath),
      orderId1 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory/1"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("1")))),
      NoKey <-: ItemAttached(workflow.id, None, bAgentPath),
      orderId1 <-: OrderStarted,
      orderId1 <-: OrderStderrWritten(s"Deleted $aDirectory/1\n"),
      orderId1 <-: OrderFinished,
      orderId1 <-: OrderDeleted,

      orderId2 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory/2"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("2")))),
      orderId2 <-: OrderStarted,
      orderId2 <-: OrderStderrWritten(s"Deleted $aDirectory/2\n"),
      orderId2 <-: OrderFinished,
      orderId2 <-: OrderDeleted,

      orderId3 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory/3"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("3")))),
      orderId3 <-: OrderStarted,
      orderId3 <-: OrderStderrWritten(s"Deleted $aDirectory/3\n"),
      orderId3 <-: OrderFinished,
      orderId3 <-: OrderDeleted,

      orderId4 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory/4"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("4")))),
      orderId4 <-: OrderStarted,
      NoKey <-: UnsignedSimpleItemChanged(bFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
      NoKey <-: ItemAttachable(orderWatchPath, aAgentPath),
      NoKey <-: ItemAttached(orderWatchPath, Some(ItemRevision(1)), aAgentPath),
      orderId4 <-: OrderStderrWritten(s"Deleted $aDirectory/4\n"),
      orderId4 <-: OrderFinished,
      orderId4 <-: OrderDeleted,

      orderId5 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory/5"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("5")))),
      orderId5 <-: OrderStarted,
      orderId5 <-: OrderStderrWritten(s"Deleted $bDirectory/5\n"),
      orderId5 <-: OrderFinished,
      orderId5 <-: OrderDeleted,

      orderId6 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory/6"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStderrWritten(s"Deleted $bDirectory/6\n"),
      orderId6 <-: OrderFinished,
      orderId6 <-: OrderDeleted,
      // And again
      orderId6 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory/6"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStderrWritten(s"Deleted $bDirectory/6\n"),
      orderId6 <-: OrderFinished,
      orderId6 <-: OrderDeleted,

      orderId7 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory/7"),
          "var" -> StringValue("VAR")),
        None,
        Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("7")))),
      orderId7 <-: OrderStarted,
      orderId7 <-: OrderStderrWritten(s"Deleted $bDirectory/7\n"),
      orderId7 <-: OrderFinished,
      orderId7 <-: OrderDeleted))
  }

  private def checkAgentEvents(client: AgentClient): Unit = {
    client.login().await(99.s)
    val keyedEvents = client.eventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst))
      .await(99.s).orThrow.map(_.value)
      .flatMap(ke => Observable.fromIterable(Some(ke.event)
        .collect {
          case e: AgentReady => e.copy(timezone = "UTC", totalRunningTime = 1.s)
          case e: InventoryItemEvent if !e.key.isInstanceOf[SubagentId] => e
          case e: OrderWatchEvent => e
        }
        .map(e => ke.copy(event = e))))
      .toListL.await(99.s)
    assert(keyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: AgentReady("UTC", 1.s),
      NoKey <-: ItemAttachedToMe(aFileWatch.copy(itemRevision = Some(ItemRevision(0)))),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("1"),
        OrderId("file:FILE-WATCH:1"),
        Map("file" -> StringValue(s"$aDirectory/1"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("1")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("2"),
        OrderId("file:FILE-WATCH:2"),
        Map("file" -> StringValue(s"$aDirectory/2"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("2")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("3"),
        OrderId("file:FILE-WATCH:3"),
        Map("file" -> StringValue(s"$aDirectory/3"))),
      NoKey <-: AgentReady("UTC", 1.s),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("3")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("4"),
        OrderId("file:FILE-WATCH:4"),
        Map("file" -> StringValue(s"$aDirectory/4"))),
      NoKey <-: ItemAttachedToMe(bFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("4")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("5"),
        OrderId("file:FILE-WATCH:5"),
        Map("file" -> StringValue(s"$bDirectory/5"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("5")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("6"),
        OrderId("file:FILE-WATCH:6"),
        Map("file" -> StringValue(s"$bDirectory/6"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("6")),
      // and again
      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("6"),
        OrderId("file:FILE-WATCH:6"),
        Map("file" -> StringValue(s"$bDirectory/6"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("6")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("7"),
        OrderId("file:FILE-WATCH:7"),
        Map("file" -> StringValue(s"$bDirectory/7"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("7"))))
  }
}

object FileWatch2Test
{
  private val aAgentPath = AgentPath("AGENT-A")
  private val bAgentPath = AgentPath("AGENT-B")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      TestJob.execute(bAgentPath),
      DeleteFileJob.execute(bAgentPath),
      TestJob.execute(bAgentPath)),
    orderPreparation = OrderPreparation(
      OrderParameterList(
        Seq(
          OrderParameter.Final("var",
            MkString/*force non-constant early evaluation*/(StringConstant("VAR")))),
        allowUndeclared = true)))


  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
}
