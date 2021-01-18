package js7.tests

import cats.syntax.flatMap._
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.perSecondString
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.akkautils.Akkas
import js7.common.commandline.CommandLineArguments
import js7.common.log.ScribeUtils
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.controller.client.AkkaHttpControllerApi.admissionToApiResource
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderForked, OrderProcessed, OrderProcessingStarted, OrderRemoved, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import js7.proxy.ControllerApi
import js7.proxy.data.event.EventAndState
import js7.tests.TestAddOrders.{Settings, Statistics, StatisticsBuilder, _}
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class TestAddOrders private(controllerApi: ControllerApi, settings: Settings)
{
  import settings.{orderCount, workflowPath}

  private def run(onOrdersAdded: FiniteDuration => Task[Unit]): Task[Checked[Statistics]] =
    Task.defer {
      val orderIds = 1 to orderCount map toOrderId
      val isOurOrder = orderIds.toSet
      val observationStarted = MVar.empty[Task, Deadline]().memoize

      val awaitOrderCompletion = controllerApi
        .eventAndStateObservable()
        .doOnStart(_ => observationStarted.flatMap(_.put(now)))
        .collect {
          case EventAndState(Stamped(eventId, ts, KeyedEvent(orderId: OrderId, event: OrderEvent)), _, _)
            if isOurOrder(orderId.root) =>
            Stamped(eventId, ts, orderId <-: event)
        }
        .scan(new StatisticsBuilder)((s, stamped) => s.count(stamped))
        .takeWhileInclusive(_.removedOrderCount < orderCount)
        .lastL
        .flatMap(statisticsBuilder =>
          observationStarted.flatMap(_.read)
            .map(_.elapsed)
            .map(statisticsBuilder.toStatistics))
        .map { statistics =>
          if (statistics.completedOrderCount != orderCount)
            Left(Problem.pure(s"eventAndStateObservable terminated unexpectedly, " +
              s"${statistics.completedOrderCount} orders completed"))
          else
            Right(statistics)
        }

      val add = observationStarted.flatMap(_.read)
        .flatMap(since =>
          addOrders(Observable.fromIterable(orderIds))
            .flatTap(result => onOrdersAdded(since.elapsed).unless(result.isLeft)))

      // TODO Deadlock wenn Workflow unbekannt ist, weil awaitOrderCompletion nicht endet
      Task.parMap2(awaitOrderCompletion, add)((a, b) => b >> a)
    }

  private def addOrders(orderIds: Observable[OrderId]): Task[Checked[Unit]] = {
    controllerApi
      .addOrders(
        orderIds.map(FreshOrder(_, workflowPath)))
      .flatMapT(_ =>
        controllerApi.removeOrdersWhenTerminated(orderIds))
      .rightAs(())
  }
}

object TestAddOrders
{
  private val defaultUserAndPassword = UserAndPassword(UserId("demo"), SecretString("demo"))

  def main(args: Array[String]): Unit = {
    ScribeUtils.coupleScribeWithSlf4j()
    val settings = parseArguments(args.toSeq)

    def logAddOrderDuration(duration: FiniteDuration) = Task {
      println(perSecondString(duration, settings.orderCount, "orders added"))
    }

    val since = now
    run(settings, logAddOrderDuration)
      .runToFuture
      .awaitInfinite
      match {
        case Left(problem) =>
          println(problem.toString)
          System.exit(1)

        case Right(statistics) =>
          statistics.logLines foreach println
          println(s"${since.elapsed.pretty} total")
    }
  }

  private[tests] def parseArguments(args: Seq[String]): Settings =
    CommandLineArguments.parse(args) { a =>
      val userAndPassword = a.optionAs[UserId]("--user=")
        .fold(defaultUserAndPassword)(userId =>
          UserAndPassword(
            userId,
            a.as[SecretString]("--password="/*TODO Should not be a command line argument*/)))
      Settings(
        a.as[WorkflowPath]("--workflow="),
        a.as[Int]("--count=", 1),
        a.seqAs[Uri]("--controller=")
          .map(Admission(_, Some(userAndPassword))))
    }

  private[tests] def run(settings: Settings, onOrdersAdded: FiniteDuration => Task[Unit])
  : Task[Checked[Statistics]] =
    Akkas.actorSystemResource("TestAddOrders")
      .flatMap(actorSystem =>
        ControllerApi.resource(
          settings.admissions.map(admissionToApiResource(_)(actorSystem))))
      .use(controllerApi =>
        new TestAddOrders(controllerApi, settings).run(onOrdersAdded))

  private def toOrderId(i: Int) = OrderId(s"TestAddOrders-$i")

  private[tests] final case class Settings(
    workflowPath: WorkflowPath,
    orderCount: Int,
    admissions: Seq[Admission])
  {
    assertThat(admissions.nonEmpty)
  }

  private[tests] final case class Statistics(
    duration: FiniteDuration,
    completedOrderCount: Int,
    totalOrderDuration: FiniteDuration,
    maximumOrderDuration: FiniteDuration,
    forkedOrderCount: Int,
    processedCount: Int,
    totalProcessDuration: FiniteDuration,
    maximumProcessDuration: FiniteDuration)
  {
    def totalOrderCount = completedOrderCount + forkedOrderCount

    def logLines: Seq[String] = Seq(
      perSecondString(duration, completedOrderCount, "main orders"),
      perSecondString(duration, totalOrderCount, "orders"),
      perSecondString(duration, processedCount, "processes"),
      s"∅ ${(totalOrderDuration / completedOrderCount).pretty} order duration, " +
        s"longest is ${maximumOrderDuration.pretty}",
      s"∅ ${(totalProcessDuration / processedCount).pretty} process duration, " +
        s"longest is ${maximumProcessDuration.pretty}")
  }

  private final class StatisticsBuilder
  {
    private val orderIdToStarted = mutable.Map[OrderId, Timestamp]()
    private val orderIdToProcessingStarted = mutable.Map[OrderId, Timestamp]()
    private var _removedOrderCount = 0
    private var forkedOrderCount = 0
    private var totalOrderDuration = 0.s
    private var maximumOrderDuration = 0.s
    private var processedCount = 0
    private var totalProcessDuration = 0.s
    private var maximumProcessDuration = 0.s

    def removedOrderCount = _removedOrderCount

    def count(stamped: Stamped[KeyedEvent[OrderEvent]]): this.type = {
      import stamped.timestamp
      import stamped.value.{key => orderId}

      stamped.value.event match {
        case OrderRemoved =>
          _removedOrderCount += 1

        case _: OrderStarted =>
          orderIdToStarted(orderId) = timestamp

        case _: OrderTerminated =>
          for (start <- orderIdToStarted.remove(orderId)) {
            val duration = timestamp - start
            totalOrderDuration += duration
            maximumOrderDuration = maximumOrderDuration max duration
          }

        case _: OrderProcessingStarted =>
          orderIdToProcessingStarted(orderId) = timestamp

        case _: OrderProcessed =>
          for (start <- orderIdToProcessingStarted.remove(orderId)) {
            processedCount += 1
            val duration = timestamp - start
            totalProcessDuration += duration
            maximumProcessDuration = maximumProcessDuration max duration
          }

        case OrderForked(children) =>
          forkedOrderCount += children.size

        case _ =>
      }
      this
    }

    def toStatistics(duration: FiniteDuration) = Statistics(
      duration,
      completedOrderCount = _removedOrderCount,
      totalOrderDuration = totalOrderDuration,
      maximumOrderDuration = maximumOrderDuration,
      forkedOrderCount = forkedOrderCount,
      processedCount = processedCount,
      totalProcessDuration = totalProcessDuration,
      maximumProcessDuration = maximumProcessDuration)
  }
}
