package com.sos.jobscheduler.tests.core

import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
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
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.event.GenericEventRoute
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.tests.core.GenericEventRouteTest._
import com.typesafe.config.ConfigFactory
import io.circe.Decoder
import java.net.{InetAddress, InetSocketAddress}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

final class GenericEventRouteTest extends FreeSpec with BeforeAndAfterAll with ProvideActorSystem with GenericEventRoute
{
  protected type Session = SimpleSession

  protected implicit def scheduler = Scheduler.global
  protected val config = ConfigFactory.parseString(
     """jobscheduler {
       |  auth.users {}
       |  auth.session {
       |    timeout = 1 minute
       |  }
       |  akka.shutdown-timeout = 10s
       |  webserver {
       |    verbose-error-messages = on
       |    shutdown-timeout = 10s
       |    auth {
       |      realm = "TEST Server"
       |      invalid-authentication-delay = 1s
       |      loopback-is-public = off
       |      get-is-public = on
       |      public = off
       |    }
       |    log {
       |      level = Debug
       |      response = on
       |    }
       |    event {
       |      streaming {
       |        chunk-timeout = 24h
       |        delay = 20ms
       |      }
       |    }
       |  }
       |}""".stripMargin)

  protected val gateKeeper = new GateKeeper(GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))
  protected final val sessionRegister = SessionRegister.start[SimpleSession](
    actorSystem, SimpleSession.apply, SessionRegister.TestConfig)
  private val shuttingDownPromise = Promise[Completed]()
  protected final lazy val shuttingDownFuture = shuttingDownPromise.future

  protected val eventWatch = new EventCollector.ForTest()

  private lazy val route = pathSegments("event")(
    new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = MasterKeyedEventJsonCodec/*Example for test*/
      def eventWatchFor(user: SimpleUser) = Task.pure(Right(eventWatch))
      override def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true
    }.route)

  private lazy val server = new AkkaWebServer with AkkaWebServer.HasUri {
    protected implicit def actorSystem = GenericEventRouteTest.this.actorSystem
    protected val config = GenericEventRouteTest.this.config
    protected val scheduler = GenericEventRouteTest.this.scheduler
    protected val bindings = WebServerBinding.Http(new InetSocketAddress(InetAddress.getLoopbackAddress, findFreeTcpPort())) :: Nil
    protected def newRoute(binding: WebServerBinding) = AkkaWebServer.BoundRoute(route)
  }

  private lazy val api = new AkkaHttpClient {
    protected val actorSystem = GenericEventRouteTest.this.actorSystem
    protected val baseUri = server.localUri
    protected val uriPrefixPath = ""
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

  "Read event stream with getLinesObservable" - {
    "empty, timeout=0" in {
      val observable = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(0.s)))
      assert(observable.toListL.await(99.s) == Nil)
    }

    "empty, timeout > 0" in {
      val t = now
      val observable = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(100.ms)))
      assert(observable.toListL.await(99.s) == Nil)
      assert(t.elapsed >= 90.ms)
    }

    "Sporadic events" in {
      eventWatch.addStamped(TestEvents(0))

      val observed = mutable.Buffer[Stamped[KeyedEvent[Event]]]()
      val observableCompleted = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
        .foreach(observed.+=)
      waitForCondition(9.s, 1.ms) { observed.size == 1 }
      assert(observed(0) == TestEvents(0))

      eventWatch.addStamped(TestEvents(1))
      waitForCondition(9.s, 1.ms) { observed.size == 2 }
      assert(observed(1) == TestEvents(1))

      observableCompleted.cancel()
    }

    "Fetch events with repeated GET requests" - {  // Similar to EventRouteTest
      "(Add more events)" in {
        TestEvents.drop(2) foreach eventWatch.addStamped
      }

      "/event?limit=3&after=30 continue" in {
        val stampedSeq = getEventsByUri("/event?limit=3&after=30")
        assert(stampedSeq.head.eventId == 40)
        assert(stampedSeq.last.eventId == 60)
      }

      "/event?limit=3&after=60 continue" in {
        val stampedSeq = getEventsByUri("/event?limit=3&after=60")
        assert(stampedSeq.head.eventId == 70)
        assert(stampedSeq.last.eventId == 90)
      }

      "/event?limit=1&after=70 rewind in last chunk" in {
        val stampedSeq = getEventsByUri("/event?limit=3&after=70")
        assert(stampedSeq.head.eventId ==  80)
        assert(stampedSeq.last.eventId == 100)
      }

      "/event?limit=3&after=80 continue" in {
        val stampedSeq = getEventsByUri("/event?limit=3&after=80")
        assert(stampedSeq.head.eventId ==  90)
        assert(stampedSeq.last.eventId == 110)
      }

      "/event?limit=3&after=60 rewind to oldest" in {
        val stampedSeq = getEventsByUri("/event?limit=3&after=60")
        assert(stampedSeq.head.eventId == 70)
        assert(stampedSeq.last.eventId == 90)
      }

      "/event?limit=3&after=150 skip some events" in {
        val runningSince = now
        val stampedSeq = getEventsByUri("/event?delay=99&limit=3&after=150")
        assert(runningSince.elapsed < 1.s)  // Events must have been returned immediately
        assert(stampedSeq.head.eventId == 160)
        assert(stampedSeq.last.eventId == 180)
      }

      "/event?after=180 no more events" in {
        assert(getEventsByUri("/event?timeout=0&after=180").isEmpty)
      }

      "/event?after=180 no more events, with timeout" in {
        val runningSince = now
        assert(getEventsByUri("/event?after=180&timeout=0.2").isEmpty)
        assert(runningSince.elapsed >= 200.millis)
      }
    }

    "Fetch EventIds" in {
      assert(getLinesObservable[EventId]("/event?eventIdOnly=true&limit=3&after=30") == 40 :: 50 :: 60 :: Nil)
    }

    "shuttingDownFuture completes observable" in {
      val observableCompleted = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
        .foreach { _ => }
      sleep(10.ms)
      assert(!observableCompleted.isCompleted)

      // Shutdown service
      shuttingDownPromise.success(Completed)
      observableCompleted.await(1.s)
    }
  }

  private def getEventObservable(eventRequest: EventRequest[Event])
  : Observable[Stamped[KeyedEvent[Event]]] =
    api.getLinesObservable[Stamped[KeyedEvent[Event]]](
      "/" + encodePath("event") + encodeQuery(eventRequest.toQueryParameters)
    ).await(99.s)

  private def getEventsByUri(uri: String): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    getLinesObservable[Stamped[KeyedEvent[OrderEvent]]](uri)

  private def getLinesObservable[A: Decoder: TypeTag](uri: String): Seq[A] =
    Observable.fromTask(api.getLinesObservable[A](uri))
      .flatten
      .toListL
      .await(99.s)
}

object GenericEventRouteTest
{
  private val TestEvents = for (i <- 1 to 18) yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("/test") ~ "VERSION", None, Map.empty))
}
