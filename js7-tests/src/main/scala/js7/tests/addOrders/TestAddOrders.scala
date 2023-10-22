package js7.tests.addOrders

import fs.Stream
import cats.syntax.flatMap.*
import com.typesafe.config.ConfigFactory
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.tests.addOrders.TestAddOrders.*
import monix.catnap.MVar
import cats.effect.IO
import cats.effect.Fiber
import monix.execution.Scheduler.Implicits.traced
import fs2.Stream
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class TestAddOrders private(controllerApi: ControllerApi, settings: Settings):
  import settings.{orderCount, workflowPath}

  /**
    * @return Tuple with
    *         - Public Observable returning the duration of adding all orders
    *         - Public Observable returning the changed number of main orders.
    *         - The complete IO returning `Statistics`.
    */
  private def run(): (Stream[IO, FiniteDuration], Stream[IO, Statistics], IO[Checked[Statistics]]) =
    val allOrdersAddedSubject = PublishSubject[FiniteDuration]()
    val statisticsSubject = PublishSubject[Statistics]()
    val io = IO.defer:
      val orderIds = 1 to orderCount map toOrderId
      val isOurOrder = orderIds.toSet
      val observationStarted = MVar.empty[IO, Deadline]().memoize

      val awaitOrderCompletion = controllerApi
        .eventAndStateStream()
        .doOnStart(_ => observationStarted.flatMap(_.put(now)))
        .scan(new StatisticsBuilder(isOurOrder, statisticsSubject))((s, es) =>
          s.count(es.stampedEvent))
        .takeWhileInclusive(_.deletedOrderCount < orderCount)
        .lastL
        .map(_.toStatistics)
        .map { statistics =>
          if statistics.completedOrderCount != orderCount then
            Left(Problem.pure("eventAndStateStream terminated unexpectedly, " +
              s"${statistics.completedOrderCount} orders run"))
          else
            Right(statistics)
        }

      val add = observationStarted.flatMap(_.read)
        .flatMap(since =>
          addOrders(Stream.fromIterable(orderIds))
            .flatTap(result =>
              IO.fromFuture(allOrdersAddedSubject.onNext(since.elapsed)).void
                .unless(result.isLeft)))

      IO.parMap2(
        awaitOrderCompletion.map(_.orThrow/*abort parMap2*/),
        add.map(_.orThrow)
      )((statistics, _) => statistics)
        .materialize.map(Checked.fromTry)
    (allOrdersAddedSubject, statisticsSubject, io)

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
  : IO[Checked[Statistics]] =
    val config = ConfigFactory.systemProperties.withFallback(ProxyConfs.defaultConfig)
    Pekkos.actorSystemResource("TestAddOrders", config)
      .flatMap(actorSystem =>
        ControllerApi.resource(
          admissionsToApiResource(settings.admissions)(actorSystem),
          ProxyConfs.fromConfig(config)))
      .use { controllerApi =>
        val testAddOrders = new TestAddOrders(controllerApi, settings)
        val (allOrdersAddedObservable, statisticsObservable, runOrdersIO) = testAddOrders.run()
        allOrdersAddedObservable.foreach(onOrdersAdded)
        statisticsObservable foreach onStatisticsUpdate
        runOrdersIO
      }

  private def toOrderId(i: Int) = OrderId(s"TestAddOrders-$i")
