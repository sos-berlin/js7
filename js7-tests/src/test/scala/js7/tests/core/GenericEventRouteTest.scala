package js7.tests.core

import io.circe.Decoder
import izumi.reflect.Tag
import java.net.{InetAddress, InetSocketAddress}
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.configutils.Configs.*
import js7.base.io.https.HttpsConfig
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.web.Uri
import js7.base.web.Uris.{encodePath, encodeQuery}
import js7.common.http.PekkoHttpClient
import js7.common.http.PekkoHttpClient.HttpException
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.PekkoHttpUtils
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.common.pekkoutils.{Pekkos, ProvideActorSystem}
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.controller.ControllerState
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderAdded
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import js7.journal.watch.{JournalEventWatch, SimpleEventCollector}
import js7.journal.web.GenericEventRoute
import js7.tests.core.GenericEventRouteTest.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

final class GenericEventRouteTest
extends OurTestSuite, BeforeAndAfterAll, ProvideActorSystem, GenericEventRoute
{
  protected type OurSession = SimpleSession

  protected def actorRefFactory = actorSystem
  private implicit def implicitActorSystem: ActorSystem = actorSystem
  protected implicit def scheduler: Scheduler = Scheduler.traced

  protected val config = config"""
    js7 {
      auth.users {}
      auth.session {
        timeout = 1 minute
      }
      pekko.shutdown-timeout = 10s
      web.chunk-size = 1MiB
      web.server {
        verbose-error-messages = on
        shutdown-timeout = 10s
        auth {
          https-client-authentication = off
          realm = "TEST Server"
          invalid-authentication-delay = 1s
          loopback-is-public = off
          get-is-public = on
          public = off
        }
        log {
          level = Debug
          response = on
        }
        services {
          event {
            streaming {
              chunk-timeout = 24h
              delay = 20ms
            }
          }
        }
      }
    }"""

  protected lazy val gateKeeper = new GateKeeper(WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))
  protected final lazy val sessionRegister = SessionRegister.forTest(
    actorSystem, SimpleSession.apply, SessionRegister.TestConfig)
  private val shuttingDown = Promise[Deadline]()
  protected val whenShuttingDown = shuttingDown.future

  private lazy val eventCollector = SimpleEventCollector[OrderEvent]().closeWithCloser
  protected val eventWatch: JournalEventWatch = eventCollector.eventWatch

  private lazy val allocatedServer = PekkoWebServer
    .resource(
      Seq(
        WebServerBinding.Http(
          new InetSocketAddress(InetAddress.getLoopbackAddress, findFreeTcpPort()))),
      config,
      _ => PekkoWebServer.BoundRoute.simple(
        pathSegments("event")(
          new GenericEventRouteProvider {
            def keyedEventTypedJsonCodec = ControllerState.keyedEventJsonCodec /*Example for test*/
          }.route)))
    .toAllocated
    .await(99.s)

  private lazy val api = new PekkoHttpClient {
    protected val actorSystem = GenericEventRouteTest.this.actorSystem
    protected val baseUri = allocatedServer.allocatedThing.localUri
    protected val name = "GenericEventRouteTest"
    protected val uriPrefixPath = ""
    protected def httpsConfig = HttpsConfig.empty
  }

  private implicit val noSessionToken: Task[Option[SessionToken]] = Task.pure(None)

  override def beforeAll() = {
    super.beforeAll()
    PekkoHttpUtils.avoidLazyObjectInitializationDeadlock()
    allocatedServer
  }

  override def afterAll() = {
    allocatedServer.release.await(99.s)
    Pekkos.terminateAndWait(actorSystem, 99.s)
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
      eventCollector.addStamped(TestEvents(0))

      val observed = mutable.Buffer[Stamped[KeyedEvent[Event]]]()
      val observableCompleted = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
        .foreach(observed += _)
      waitForCondition(9.s, 1.ms) { observed.size == 1 }
      assert(observed(0) == TestEvents(0))

      eventCollector.addStamped(TestEvents(1))
      waitForCondition(9.s, 1.ms) { observed.size == 2 }
      assert(observed(1) == TestEvents(1))

      observableCompleted.cancel()
    }

    "Fetch events with repeated GET requests" - {  // Similar to EventRouteTest
      "(Add more events)" in {
        TestEvents.drop(2) foreach eventCollector.addStamped
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

      "Repeatedly" in {
        for _ <- 1 to 1000 do {
          locally {
            val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=30"))
            assert(stampedSeq.head.eventId == 40)
            assert(stampedSeq.last.eventId == 60)
          }
          locally {
            val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=60"))
            assert(stampedSeq.head.eventId == 70)
            assert(stampedSeq.last.eventId == 90)
          }
        }
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
        assert(runningSince.elapsed < 3.s)  // Events must have been returned immediately
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

    "Fetch EventIds while active is rejected" in {
      assert(eventWatch.isActiveNode)
      val response = intercept[HttpException](
        getDecodedLinesObservable[EventId](Uri("/event?onlyAcks=true&timeout=0")))
      assert(response.problem == Some(AckFromActiveClusterNodeProblem))
    }

    "Fetch EventIds while passive" in {
      val wasActive = eventWatch.isActiveNode
      eventWatch.isActiveNode = false

      assert(getDecodedLinesObservable[EventId](Uri("/event?onlyAcks=true&timeout=0")) == Seq(180L))

      eventWatch.isActiveNode = wasActive
    }

    "Fetch EventIds with heartbeat" in {
      val wasActive = eventWatch.isActiveNode
      eventWatch.isActiveNode = false

      val uri = Uri("/event?onlyAcks=true&heartbeat=0.1&timeout=3")
      val events = Observable.fromTask(api.getDecodedLinesObservable[EventId](uri))
        .flatten
        .take(3)
        .toListL
        .await(99.s)
      assert(events == Seq(180, 180/*heartbeat*/, 180/*heartbeat*/))

      eventWatch.isActiveNode = wasActive
    }

    //"cancel PekkoHttpClient request" in {
    //  for (i <- 1 to 16/*below Pekko's max-open-requests, see js7.conf, otherwise the pool will overflow and block*/) {
    //    logger.debug(s"cancel #$i")
    //    val future = getEvents(EventRequest.singleClass[Event](after = eventWatch.lastAddedEventId, timeout = Some(99.s)))
    //      .runToFuture
    //    sleep(10.ms)
    //    assert(!future.isCompleted)
    //    future.cancel()
    //  }
    //}

    "whenShuttingDown" - {
      "completes a running observable" in {
        val started = Promise[Unit]()
        val observableCompleted = getEventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
          .doOnStart(_ => Task {
            started.success(())
          })
          .completedL.runToFuture
        started.future await 9.s
        assert(!observableCompleted.isCompleted)
        // Shut down service
        shuttingDown.success(now)
        observableCompleted await 99.s
      }

      "completes a observable request, before observable started" in {
        // Shut down service, try again, in case the previous test failed
        shuttingDown.trySuccess(now)
        val observableCompleted = getEventObservable(EventRequest.singleClass[Event](after = eventWatch.lastAddedEventId, timeout = Some(99.s)))
          .completedL.runToFuture
        // Previous test has already shut down the service
        observableCompleted await 99.s
      }
    }
  }

  private def getEventObservable(eventRequest: EventRequest[Event]): Observable[Stamped[KeyedEvent[Event]]] =
    getEvents(eventRequest).await(99.s)

  private def getEvents(eventRequest: EventRequest[Event]): Task[Observable[Stamped[KeyedEvent[Event]]]] = {
    import ControllerState.keyedEventJsonCodec
    api.getDecodedLinesObservable[Stamped[KeyedEvent[Event]]](
      Uri("/" + encodePath("event") + encodeQuery(eventRequest.toQueryParameters)),
      responsive = true)
  }

  private def getEventsByUri(uri: Uri): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    getDecodedLinesObservable[Stamped[KeyedEvent[OrderEvent]]](uri)

  private def getDecodedLinesObservable[A: Decoder: Tag](uri: Uri): Seq[A] =
    Observable.fromTask(api.getDecodedLinesObservable[A](uri, responsive = true))
      .flatten
      .toListL
      .await(99.s)
}


object GenericEventRouteTest
{
  private val TestEvents = for i <- 1 to 18 yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("test") ~ "VERSION"))
  private val ExtraEvent =
    Stamped(EventId(10 * 99), Timestamp.ofEpochMilli(999),
      OrderId(99.toString) <-: OrderAdded(WorkflowPath("test") ~ "VERSION"))
}
