package com.sos.jobscheduler.common.akkahttp

import akka.stream.ActorMaterializer
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.closeableIteratorToObservable
import com.sos.jobscheduler.common.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends FreeSpec {

  "closeableIteratorToAkkaSource" in {
    val actorSystem = Akkas.newActorSystem("StreamingSupportTest")
    implicit val materializer = ActorMaterializer.create(actorSystem)
    val iterator = new CloseableIterator[Int] {
      var closed = false
      private val it = Iterator.from(1) take 3
      def close() = closed = true
      def hasNext = it.hasNext
      def next() = it.next()
    }
    val result = mutable.Buffer[Int]()
    assert(!iterator.closed)
    closeableIteratorToObservable(iterator).toAkkaSource.runForeach(result.+=) await 99.s
    assert(result == List(1, 2, 3))
    assert(iterator.closed)
    actorSystem.terminate()
  }
}
