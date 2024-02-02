package js7.tests.addOrders

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import com.typesafe.config.ConfigFactory
import js7.base.catsutils.CatsEffectExtensions.catchAsChecked
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.MVar
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.tests.addOrders.TestAddOrders.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}
import fs2.Stream

final class TestAddOrders private(controllerApi: ControllerApi, settings: Settings):

  import settings.{orderCount, workflowPath}

  /**
    * @return Tuple with
    *         - Public Observable returning the duration of adding all orders
    *         - Public Observable returning the changed number of main orders.
    *         - The complete IO returning `Statistics`.
    */
  private def run
  : IO[(Stream[IO, FiniteDuration], Stream[IO, Statistics], IO[Checked[Statistics]])] =
    for
      allOrdersAddedQueue <- Queue.bounded[IO, Option[FiniteDuration]](100)
      statisticsQueue <- Queue.bounded[IO, Option[Statistics]](100)
      allOrdersAddedStream = Stream.fromQueueNoneTerminated(allOrdersAddedQueue)
      statisticsStream = Stream.fromQueueNoneTerminated(statisticsQueue)
      untilCompleted =
        run2(o => allOrdersAddedQueue.offer(Some(o)), o => statisticsQueue.offer(Some(o)))
          .guarantee(IO
            .both(
              allOrdersAddedQueue.offer(None),
              statisticsQueue.offer(None))
            .void)
    yield
      (allOrdersAddedStream, statisticsStream, untilCompleted)

  private def run2(
    onAllOrdersAdded: FiniteDuration => IO[Unit],
    onStatistics: Statistics => IO[Unit])
  : IO[Checked[Statistics]] =
    IO.defer:
      val orderIds = 1 to orderCount map toOrderId
      val isOurOrder = orderIds.toSet
      val observationStarted = MVar.empty[IO, Deadline].unsafeMemoize

      val statisticsBuilder = new StatisticsBuilder(isOurOrder, onStatistics)
      val untilCompleted = controllerApi
        .eventAndStateStream()
        .onStart(observationStarted.flatMap(_.put(now)))
        .evalTap(_ => onStatistics(statisticsBuilder.toStatistics)) // Initial value
        .evalScan(statisticsBuilder): (s, es) =>
          s.count(es.stampedEvent)
        .takeThrough(_.deletedOrderCount < orderCount)
        .compile.last
        .flatMap:
          case None => IO.raiseError(new NoSuchElementException("TestAddOrders isEmpty"))
          case Some(o) => IO.pure(o)
        .map(_.toStatistics)
        .map: statistics =>
          if statistics.completedOrderCount != orderCount then
            Left(Problem.pure("eventAndStateStream terminated unexpectedly, " +
              s"${statistics.completedOrderCount} orders run"))
          else
            Right(statistics)

      val add = observationStarted
        .flatMap(_.read)
        .flatMap: since =>
          addOrders(Stream.iterable(orderIds))
            .flatTap: result =>
              IO.unlessA(result.isLeft):
                onAllOrdersAdded(since.elapsed)

      IO
        .both(
          untilCompleted.map(_.orThrow),
          add.map(_.orThrow))
        .map(_._1)
        .catchAsChecked

  private def addOrders(orderIds: Stream[IO, OrderId]): IO[Checked[Unit]] =
    controllerApi
      .addOrders(
        orderIds.map(FreshOrder(_, workflowPath)))
      .flatMapT(_ =>
        controllerApi.deleteOrdersWhenTerminated(orderIds))
      .rightAs(())


object TestAddOrders:

  private[addOrders] def run(
    settings: Settings,
    onStatisticsUpdate: Statistics => Unit,
    onOrdersAdded: FiniteDuration => Unit)
    (using IORuntime)
  : IO[Checked[Statistics]] =
    val config = ConfigFactory.systemProperties.withFallback(ProxyConfs.defaultConfig)
    Pekkos.actorSystemResource("TestAddOrders", config)
      .flatMap: actorSystem =>
        ControllerApi.resource(
          admissionsToApiResource(settings.admissions)(actorSystem),
          ProxyConfs.fromConfig(config))
      .use: controllerApi =>
        val testAddOrders = new TestAddOrders(controllerApi, settings)
        for
          tuple <- testAddOrders.run
          (allOrdersAddedStream, statisticsStream, untilCompleted) = tuple

          _ <- allOrdersAddedStream
            .foreach(o => IO(onOrdersAdded(o)))
            .compile.drain
            .start

          _ <- statisticsStream
            .foreach(o => IO(onStatisticsUpdate(o)))
            .compile.drain
            .start
          statistics <- untilCompleted
        yield statistics

  private def toOrderId(i: Int) = OrderId(s"TestAddOrders-$i")
