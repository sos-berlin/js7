package js7.base.monixutils

import js7.base.monixutils.MonixBase.syntax.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import cats.effect.IO
import monix.execution.schedulers.TestScheduler
import fs2.Stream
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class InsertHeartbeatsOnSlowUpstreamTest extends OurTestSuite:
  private implicit val scheduler: TestScheduler = TestScheduler()
  private val heartbeat = -1

  "insertHeartbeatsOnSlowUpstream" in:
    assert(runStream(List(1, 2, 100, 200, 100)) ==
      List(1, 2, heartbeat, 100, heartbeat, heartbeat, 200, heartbeat, 100))

  "insertHeartbeatsOnSlowUpstream from start" in:
    assert(runStream(List(200, 1, 2, 100)) ==
      List(heartbeat, heartbeat, 200, 1, 2, heartbeat, 100))

  private def runStream(milliseconds: Seq[Int]): Seq[Int] =
    val future = Stream.fromIterable(milliseconds)
      .mapEval(i => IO(i).delayBy(i.ms))
      .insertHeartbeatsOnSlowUpstream(80.ms, heartbeat)
      .toListL
      .runToFuture
    for  _ <- 1 to 100 do scheduler.tick(80.ms)
    Await.result(future, 0.s)
