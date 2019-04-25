package com.sos.jobscheduler.common.akkautils

import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.akkautils.CatchingActorTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class CatchingActorTest extends FreeSpec with BeforeAndAfterAll {

  private implicit lazy val actorSystem = ActorSystem("CatchingActorTest")

  override protected def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "Normal stop" in {
    val finished = Promise[Completed]()
    actorSystem.actorOf(Props { new TestActor(finished) })
    finished.future await 99.s
  }

  "Unexpected stop" in {
    val (a, terminated) = CatchingActor.actorOf(props, loggingEnabled = false)
    a ! "STOP"
    Await.ready(terminated, 99.seconds).value.get.failed.get.getClass shouldBe classOf[CatchingActor.StoppedException]
  }

  "Exception" in {
    val (a, terminated) = CatchingActor.actorOf(props, loggingEnabled = false)
    val throwable = new IllegalArgumentException("TEST") with NoStackTrace
    a ! throwable
    val gotThrowable = Await.ready(terminated, 99.seconds).value.get.failed.get
    assert(gotThrowable.isInstanceOf[CatchingSupervisorStrategy.ActorTerminatedException])
    assert(gotThrowable.getCause eq throwable)
  }
}

object CatchingActorTest {
  private implicit val timeout = Timeout(99.seconds)

  private class TestActor(finished: Promise[Completed]) extends Actor {
    val (a, terminated) = CatchingActor.actorOf(props)
    a ! "FROM PARENT"

    def receive = {
      case "Response to FROM PARENT" =>
        assert(sender() == a)
        assert(((a ? "ASK") await  99.s) == "Response to ASK")
        a ! "END"
        terminated await 99.s shouldBe Completed
        context.stop(self)
        finished.success(Completed)
    }
  }

  private def props(promise: Promise[Completed]) = Props {
    new Actor {
      def receive = {
        case throwable: Throwable =>
          throw throwable

        case "END" =>
          promise.success(Completed)
          context.stop(self)

        case "STOP" =>
          context.stop(self)

        case o: String =>
          if (o == "FROM PARENT") assert(sender() == context.parent)
          sender() ! s"Response to $o"
      }
    }
  }
}

