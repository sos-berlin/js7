package js7.tests.addOrders

import cats.effect.std.Queue
import cats.effect.{Deferred, FiberIO, IO}
import cats.syntax.option.*
import com.typesafe.config.ConfigFactory
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.durationAndPerSecondString
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.tests.addOrders.TestAddOrders.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class TestAddOrders private(controllerApi: ControllerApi, settings: Settings):
  import settings.{orderCount, workflowPath}

  /**
    * @return Tuple with
    *         - Public Queue returning the duration of adding all orders
    *         - Public Queue returning the changed number of main orders.
    *         - The complete IO returning `Statistics`.
    */
  private def run()
  : IO[(IO[FiniteDuration], Stream[IO, Statistics], FiberIO[Checked[Statistics]])] =
    for
      allOrdersAdded <- Deferred[IO, FiniteDuration]
      statisticsQueue <- Queue.bounded[IO, Option[Statistics]](1)
      fiber <- IO.defer:
        val orderIds = 1 to orderCount map toOrderId
        val isOurOrder = orderIds.toSet
        val streamingStarted = Deferred.unsafe[IO, Deadline]
        val statisticsBuilder = StatisticsBuilder(isOurOrder, o => statisticsQueue.tryOffer(o.some))

        val awaitOrderCompletion = controllerApi
          .eventAndStateStream()
          .onStart:
            streamingStarted.complete(now).void
          .evalScan(statisticsBuilder): (s, es) =>
            s.count(es.stampedEvent).as(s)
          .takeThrough(_.deletedOrderCount < orderCount)
          .compile
          .last.map(_.get) /*always nonEmpty*/
          .map(_.toStatistics)
          .map: statistics =>
            if statistics.completedOrderCount != orderCount then
              Left(Problem.pure("eventAndStateStream terminated unexpectedly, " +
                s"${statistics.completedOrderCount} orders run"))
            else
              Right(statistics)

        val add: IO[Checked[Unit]] =
          streamingStarted.get.flatMap: since =>
            addOrders(orderIds).flatTap:
              case Right(_) => allOrdersAdded.complete(since.elapsed).void
              case Left(_) => IO.unit

        IO
          .both(
            awaitOrderCompletion.map(_.orThrow),
            add.map(_.orThrow))
          .map(_._1)
          .attempt.map(Checked.fromThrowableEither)
          .flatTap: _ =>
            statisticsQueue.offer(None)
          .start
    yield
      (allOrdersAdded.get, Stream.fromQueueNoneTerminated(statisticsQueue, 1), fiber)

  private def addOrders(orderIds: Iterable[OrderId]): IO[Checked[Unit]] =
    logger.debugIO:
      controllerApi.addOrders:
        Stream.iterable(orderIds).map:
          FreshOrder(_, workflowPath, deleteWhenTerminated = true)
      .rightAs(())


object TestAddOrders:

  private[addOrders] val logger = Logger[this.type]
  private val ClearLine = "\u001B[K"

  private[addOrders] def run(settings: Settings, logToStdout: Boolean = false)
  : IO[Checked[Statistics]] =

    def myPrint(line: => String): Unit =
      if logToStdout then
        println(line)

    def onOrdersAdded(duration: FiniteDuration) =
      IO:
        myPrint("\r" + ClearLine +
          durationAndPerSecondString(duration, settings.orderCount, "orders added") +
          ClearLine + "\n" + ClearLine)

    def onStatisticsUpdate(statistics: Statistics) =
      IO.whenA(statistics.totalOrderCount > 0):
        IO:
          if logToStdout then
            print(s"\r${statistics.toLine}  $ClearLine")
          else
            logger.info(s"${statistics.lastOrderCount} orders")

    run2(settings, onOrdersAdded, onStatisticsUpdate)
      .flatTap:
        case Left(problem) => IO:
          myPrint(s"\r$ClearLine\n")

        case Right(statistics) => IO:
          myPrint(s"\r$ClearLine\n" +
            statistics.logLines.map(line => s"$line$ClearLine\n").mkString)

  private def run2(
      settings: Settings,
      onOrdersAdded: FiniteDuration => IO[Unit],
      onStatisticsUpdate: Statistics => IO[Unit])
    : IO[Checked[Statistics]] =
      val config = ConfigFactory.systemProperties.withFallback(ProxyConfs.defaultConfig)
      Pekkos.actorSystemResource("TestAddOrders", config)
        .flatMap: actorSystem =>
          ControllerApi.resource(
            admissionsToApiResource(settings.admissions)(using actorSystem),
            ProxyConfs.fromConfig(config))
        .use: controllerApi =>
          val testAddOrders = new TestAddOrders(controllerApi, settings)
          testAddOrders.run()
            .flatMap: (whenAllOrdersAdded, statisticsQueue, running) =>
              whenAllOrdersAdded.flatMap(onOrdersAdded)
                .*>(statisticsQueue.foreach(onStatisticsUpdate).compile.drain)
                .*>(running.joinStd)

  private def toOrderId(i: Int) = OrderId(s"TestAddOrders-$i")
