package js7.tests.core

import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.ProvideActorSystem
import js7.common.event.collector.EventCollector
import js7.common.http.AkkaHttpClient
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.http.Uris.{encodePath, encodeQuery}
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.core.event.GenericEventRoute
import js7.data.event.{Event, EventId, EventRequest, EventSeqTornProblem, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderAdded
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import js7.master.data.events.MasterKeyedEventJsonCodec
import js7.tests.core.GenericEventRouteTest._
import com.typesafe.config.ConfigFactory
import io.circe.Decoder
import java.net.{InetAddress, InetSocketAddress}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

final class GenericEventRouteTest extends AnyFreeSpec with BeforeAndAfterAll with ProvideActorSystem with GenericEventRoute
{
  protected type Session = SimpleSession

  protected implicit def scheduler = Scheduler.global
  protected val config = ConfigFactory.parseString(
     """js7 {
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
       |    services {
       |      event {
       |        streaming {
       |          chunk-timeout = 24h
       |          delay = 20ms
       |        }
       |      }
       |    }
       |  }
       |}""".stripMargin)

  protected val gateKeeper = new GateKeeper(GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))
  protected final val sessionRegister = SessionRegister.start[SimpleSession](
    actorSystem, SimpleSession.apply, SessionRegister.TestConfig)
  @volatile protected var isShuttingDown = false

  protected val eventWatch = new EventCollector.ForTest()  // TODO Use real JournalEventWatch

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
    protected val name = "GenericEventRouteTest"
    protected val uriPrefixPath = ""
    protected def keyStoreRef = None
    protected def trustStoreRef = None
  }

  private implicit val noSessionToken: Task[Option[SessionToken]] = Task.pure(None)

  override def beforeAll() = {
    super.beforeAll()
    server.start() await 99.s
  }

  override def afterAll() = {
    server.close()
    actorSystem.terminate() await 9.s
    super.afterAll()
  }

  "Read event stream with getDecodedLinesObservable" - {
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
        .foreach(observed += _)
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
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=30"))
        assert(stampedSeq.head.eventId == 40)
        assert(stampedSeq.last.eventId == 60)
      }

      "/event?limit=3&after=60 continue" in {
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=60"))
        assert(stampedSeq.head.eventId == 70)
        assert(stampedSeq.last.eventId == 90)
      }

      "/event?limit=1&after=70 rewind in last chunk" in {
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=70"))
        assert(stampedSeq.head.eventId ==  80)
        assert(stampedSeq.last.eventId == 100)
      }

      "/event?limit=3&after=80 continue" in {
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=80"))
        assert(stampedSeq.head.eventId ==  90)
        assert(stampedSeq.last.eventId == 110)
      }

      "/event?limit=3&after=60 rewind to oldest" in {
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=60"))
        assert(stampedSeq.head.eventId == 70)
        assert(stampedSeq.last.eventId == 90)
      }

      "/event?limit=3&after=150 skip some events" in {
        val runningSince = now
        val stampedSeq = getEventsByUri(Uri("/event?delay=99&limit=3&after=150"))
        assert(runningSince.elapsed < 1.s)  // Events must have been returned immediately
        assert(stampedSeq.head.eventId == 160)
        assert(stampedSeq.last.eventId == 180)
      }

      "/event?after=180 no more events" in {
        assert(getEventsByUri(Uri("/event?timeout=0&after=180")).isEmpty)
      }

      "/event?after=180 no more events, with timeout" in {
        val runningSince = now
        assert(getEventsByUri(Uri("/event?after=180&timeout=0.2")).isEmpty)
        assert(runningSince.elapsed >= 200.millis)
      }
    }

    "Fetch EventIds" in {
      assert(getDecodedLinesObservable[EventId](Uri("/event?eventIdOnly=true&limit=3&after=30")) == 40 :: 50 :: 60 :: Nil)
    }

    "Fetch EventIds with heartbeat" in {
      assert(getDecodedLinesObservable[EventId](Uri("/event?eventIdOnly=true&limit=5&after=150&heartbeat=0.1&timeout=0.5")).take(5) ==
        160 :: 170 :: 180 :: 180/*heartbeat*/ :: 180/*heartbeat*/ :: Nil)
    }

    "Fetching EventIds with after=unknown-EventId is rejected" in {
      val isKnownEventId = TestEvents.map(_.eventId).toSet
      for (unknown <- 1L to (TestEvents.last.eventId) if !isKnownEventId(unknown)) withClue(s"EventId $unknown: ") {
        val t = intercept[HttpException] {
          getDecodedLinesObservable[EventId](Uri(s"/event?eventIdOnly=true&timeout=1&after=$unknown"))
        }
        assert(t.problem contains EventSeqTornProblem(unknown, 0L))
      }
    }

    "Fetching EventIds with after=future-event is accepted" in {
      val t = now
      val result = getDecodedLinesObservable[EventId](Uri(s"/event?eventIdOnly=true&timeout=0.1&after=${TestEvents.last.eventId + 1}"))
      assert(result.isEmpty && t.elapsed > 0.1.second)
    }

    "isShuttingDown completes observable" in {
      val observableCompleted = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
        .foreach { _ => }
      sleep(10.ms)
      assert(!observableCompleted.isCompleted)

      // ShutDown service
      isShuttingDown = true
      sleep(100.ms)
      assert(!observableCompleted.isCompleted)  // isShuttingDown is only checked after an event has been received !!! (to avoid a memory leak)

      eventWatch.addStamped(ExtraEvent)
      observableCompleted.await(1.s)
    }
  }

  private def getEventObservable(eventRequest: EventRequest[Event]): Observable[Stamped[KeyedEvent[Event]]] =
    api.getDecodedLinesObservable[Stamped[KeyedEvent[Event]]](
      Uri("/" + encodePath("event") + encodeQuery(eventRequest.toQueryParameters))
    ).await(99.s)

  private def getEventsByUri(uri: Uri): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    getDecodedLinesObservable[Stamped[KeyedEvent[OrderEvent]]](uri)

  private def getDecodedLinesObservable[A: Decoder: TypeTag](uri: Uri): Seq[A] =
    Observable.fromTask(api.getDecodedLinesObservable[A](uri))
      .flatten
      .toListL
      .await(99.s)
}

object GenericEventRouteTest
{
  private val TestEvents = for (i <- 1 to 18) yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("/test") ~ "VERSION", None, Map.empty))
  private val ExtraEvent =
    Stamped(EventId(10 * 99), Timestamp.ofEpochMilli(999),
      OrderId(99.toString) <-: OrderAdded(WorkflowPath("/test") ~ "VERSION", None, Map.empty))
}
