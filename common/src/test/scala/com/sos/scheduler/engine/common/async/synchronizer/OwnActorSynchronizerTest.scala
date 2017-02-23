package com.sos.scheduler.engine.common.async.synchronizer

import akka.actor.ActorSystem
import com.sos.scheduler.engine.common.scalautil.Closers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.RichFutures
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.scalatest.FreeSpec
import scala.collection.immutable.IndexedSeq
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class OwnActorSynchronizerTest extends FreeSpec {

  "test" in {
    withCloser { implicit closer ⇒
      val actorSystem = ActorSystem("ShellProcessTaskTest") withCloser { _.shutdown() }
      import actorSystem.dispatcher
      val synchronizer =
        new OwnActorSynchronizer[Int] {
          protected def actorRefFactory = actorSystem
        }
        .closeWithCloser
      val synchronizedFuture: (⇒ Int) ⇒ Future[Int] = synchronizer
      val numbers = 1 to 100
      @volatile var critical = false
      val futureFutures: IndexedSeq[Future[Future[Int]]] =
        for (i ← numbers) yield Future {
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
