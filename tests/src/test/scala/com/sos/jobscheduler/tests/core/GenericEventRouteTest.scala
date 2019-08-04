package com.sos.jobscheduler.tests.core

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.http.Uris.{encodePath, encodeQuery}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.event.GenericEventRoute
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderStarted}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec
import com.typesafe.config.ConfigFactory
import java.net.{InetAddress, InetSocketAddress}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

final class GenericEventRouteTest extends FreeSpec with BeforeAndAfterAll with ProvideActorSystem with GenericEventRoute
{
  protected type Session = SimpleSession

  protected implicit def scheduler = Scheduler.global
  protected val config = ConfigFactory.parseString(
    """jobscheduler.auth.users {}
      |jobscheduler.auth.session {
      |  timeout = 1 minute
      |}
      |
      |jobscheduler.webserver {
      |  verbose-error-messages = on
      |  shutdown-timeout = 10s
      |  auth {
      |    realm = "TEST Server"
      |    invalid-authentication-delay = 1s
      |    loopback-is-public = off
      |    get-is-public = on
      |    public = off
      |  }
      |  log {
      |    level = Debug
      |    response = on
      |  }
      |  event {
      |    streaming {
      |      chunk-timeout = 24h
      |      delay = 20ms
      |    }
      |  }
      |}""".stripMargin)

  protected val gateKeeper = new GateKeeper(GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))
  protected final val sessionRegister = SessionRegister.start[SimpleSession](
    actorSystem, SimpleSession.apply, SessionRegister.TestConfig)
  private val closedPromise = Promise[Completed]()
  protected final lazy val closedObservable = Observable.fromFuture(closedPromise.future)

  protected val eventWatch = new EventCollector.ForTest()(scheduler)

  private lazy val route = pathSegments("TEST/events")(
    new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = MasterKeyedEventJsonCodec/*Example for test*/
      def eventWatchFor(user: SimpleUser) = Task.pure(Valid(eventWatch))
      override def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true
    }.route)

  private lazy val server = new AkkaWebServer with AkkaWebServer.HasUri {
    protected implicit def actorSystem = GenericEventRouteTest.this.actorSystem
    protected val config = ConfigFactory.empty
    protected val scheduler = GenericEventRouteTest.this.scheduler
    protected val bindings = WebServerBinding.Http(new InetSocketAddress(InetAddress.getLoopbackAddress, findFreeTcpPort())) :: Nil
    protected def newRoute(binding: WebServerBinding) = AkkaWebServer.BoundRoute(route)
  }

  private lazy val api = new AkkaHttpClient {
    protected val actorSystem = GenericEventRouteTest.this.actorSystem
    protected val baseUri = server.localUri
    protected val uriPrefixPath = "/TEST"
    protected val sessionToken = None
  }

  override def beforeAll() = {
    super.beforeAll()
    server.start() await 99.s
  }

  override def afterAll() = {
    server.close()
    actorSystem.terminate() await 9.s
    super.afterAll()
  }

  "Read events with getLinesObservable" - {
    "empty, timeout=0" in {
      assert(getEventObservable(timeout = 0.s).toListL.await(99.s) == Nil)
    }

    "empty, timeout > 0" in {
      val t = now
      assert(getEventObservable(timeout = 100.ms).toListL.await(99.s) == Nil)
      assert(t.elapsed >= 90.ms)
    }

    "Some events and closedObservable" in {
      val stamped1 = Stamped(1, OrderId("A") <-: OrderStarted)
      eventWatch.addStamped(stamped1)

      val observed = mutable.Buffer[Stamped[KeyedEvent[Event]]]()
      val observableCompleted = getEventObservable(timeout = 99.s).foreach(observed.+=)
      waitForCondition(9.s, 1.ms) { observed.size == 1 }
      assert(observed(0) == stamped1)

      val stamped2 = Stamped(2, OrderId("B") <-: OrderFinished)
      eventWatch.addStamped(stamped2)
      waitForCondition(9.s, 1.ms) { observed.size == 2 }
      assert(observed(1) == stamped2)

      closedPromise.success(Completed)
      observableCompleted.await(1.s)
    }
  }

  private def getEventObservable(timeout: FiniteDuration): Observable[Stamped[KeyedEvent[Event]]] =
    api.getLinesObservable[Stamped[KeyedEvent[Event]]](
      "/" + encodePath("TEST", "events") +
        encodeQuery(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(timeout)).toQueryParameters)
    ).await(99.s)
}

object GenericEventRouteTest {
  private val logger = Logger(getClass)
}
