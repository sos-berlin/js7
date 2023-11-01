package js7.common.pekkoutils

import js7.base.generic.Completed
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.common.pekkoutils.CatchingActorTest.*
import js7.common.pekkoutils.Pekkos.newActorSystem
import org.apache.pekko.actor.{Actor, ActorSystem, Props}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers.*
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class CatchingActorTest extends OurTestSuite with BeforeAndAfterAll {

  private implicit lazy val actorSystem: ActorSystem =
    newActorSystem("CatchingActorTest")

  override protected def afterAll() = {
    Pekkos.terminateAndWait(actorSystem, 99.s)
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
  private implicit val timeout: Timeout = Timeout(99.seconds)

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
