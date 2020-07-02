package js7.common.async.synchronizer

import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.scalautil.Futures.blockingThreadFuture
import js7.common.scalautil.Futures.implicits.RichFutures
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class OwnActorSynchronizerTest extends AnyFreeSpec {

  "test" in {
    withCloser { implicit closer =>
      val actorSystem = newActorSystem("ShellProcessTaskTest")
        .withCloser(Akkas.terminateAndWait(_, 99.s))
      import actorSystem.dispatcher
      val synchronizer =
        new OwnActorSynchronizer[Int] {
          protected def actorRefFactory = actorSystem
        }
        .closeWithCloser
      val synchronizedFuture: (=> Int) => Future[Int] = synchronizer
      val numbers = 1 to 100
      @volatile var critical = false
      val futureFutures: IndexedSeq[Future[Future[Int]]] =
        for (i <- numbers) yield blockingThreadFuture {
          synchronizedFuture {
            assert(!critical)
            critical = true
            sleep(10.ms)
            assert(critical)
            critical = false
            i
          }
        }
      assert((futureFutures map { _ flatMap identity } await 60.s) == numbers)

      synchronizer.close()
      intercept[IllegalStateException] { synchronizer { 7 } }
    }
  }
}
