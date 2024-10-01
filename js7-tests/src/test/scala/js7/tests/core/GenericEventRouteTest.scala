package js7.tests.core

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import fs2.Stream
import io.circe.Decoder
import izumi.reflect.Tag
import java.net.{InetAddress, InetSocketAddress}
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.configutils.Configs.*
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.io.https.HttpsConfig
import js7.base.monixlike.MonixLikeExtensions.{completedL, toListL, unsafeToCancelableFuture}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.Tests
import js7.base.utils.Tests.isIntelliJIdea
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
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.core.GenericEventRouteTest.*
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

final class GenericEventRouteTest
extends OurTestSuite, BeforeAndAfterAll, ProvideActorSystem, GenericEventRoute:

  protected type OurSession = SimpleSession

  private given IORuntime = ioRuntime

  protected def actorRefFactory = actorSystem
  private implicit def implicitActorSystem: ActorSystem = actorSystem

  protected val config = config"""
    js7 {
      auth.users {}
      auth.session {
        timeout = 1 minute
      }
      pekko.shutdown-timeout = 10s
      web.chunk-size = 1MiB
      web.client.prefetch = 0
      web.server.prefetch = 0
      web.server {
        verbose-error-messages = on
        shutdown-timeout = 10s
        shutdown-delay = 0s
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
              timeout = 6h
              delay = 20ms
            }
          }
        }
      }
    }
    pekko.loglevel = DEBUG
    pekko.actor.debug.autoreceive = on
    pekko.actor.debug.lifecycle = on
    pekko.actor.debug.unhandled = on
    """

  protected lazy val gateKeeper = new GateKeeper(WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))
  protected final lazy val sessionRegister = SessionRegister.forTest(
    SimpleSession.apply, SessionRegister.TestConfig)
  protected val whenShuttingDown = Deferred.unsafe

  private lazy val eventCollector = SimpleEventCollector[OrderEvent]().closeWithCloser
  protected lazy val eventWatch: JournalEventWatch = eventCollector.eventWatch

  private lazy val allocatedServer = PekkoWebServer
    .resource(
      Seq(
        WebServerBinding.Http(
          new InetSocketAddress(InetAddress.getLoopbackAddress, findFreeTcpPort()))),
      config,
      _ => PekkoWebServer.BoundRoute.simple(
        pathSegments("event")(
          genericEventRoute(ControllerState.keyedEventJsonCodec /*Example for test*/))))
    .toAllocated
    .await(99.s)

  private lazy val api = new PekkoHttpClient:
    protected val actorSystem = GenericEventRouteTest.this.actorSystem
    protected val baseUri = allocatedServer.allocatedThing.localUri
    protected val name = "GenericEventRouteTest"
    protected val uriPrefixPath = ""
    protected def httpsConfig = HttpsConfig.empty

  private implicit val noSessionToken: IO[Option[SessionToken]] = IO.pure(None)

  override def beforeAll() =
    super.beforeAll()
    PekkoHttpUtils.avoidLazyObjectInitializationDeadlock()
    allocatedServer

  override def afterAll() =
    try
      allocatedServer.release.await(99.s)
      Pekkos.terminateAndWait(actorSystem, 99.s)
    finally
      super.afterAll()

  "Read event stream with getDecodedLinesStream" - {
    "empty, timeout=0" in:
      val stream = getEventStream(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(0.s)))
      assert(stream.toListL.await(99.s) == Nil)

    "empty, timeout > 0" in:
      val t = now
      val stream = getEventStream(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(100.ms)))
      assert(stream.toListL.await(99.s) == Nil)
      assert(t.elapsed >= 90.ms)

    "Sporadic events" in:
      eventCollector.addStamped(TestEvents(0))

      val observed = mutable.Buffer[Stamped[KeyedEvent[Event]]]()
      val streamCompleted = getEventStream(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
        .foreach(o => IO:
          observed += o)
        .compile.drain
        .unsafeToCancelableFuture()
      awaitAndAssert { observed.size == 1 }
      assert(observed(0) == TestEvents(0))

      eventCollector.addStamped(TestEvents(1))
      awaitAndAssert { observed.size == 2 }
      assert(observed(1) == TestEvents(1))

      streamCompleted.cancelToFuture().await(99.s)

    "Fetch events with repeated GET requests" - {  // Similar to EventRouteTest
      "(Add more events)" in:
        TestEvents.drop(2) foreach eventCollector.addStamped

      "/event?limit=3&after=30 continue" in:
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=30"))
        assert(stampedSeq.head.eventId == 40)
        assert(stampedSeq.last.eventId == 60)

      "/event?limit=3&after=60 continue" in:
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=60"))
        assert(stampedSeq.head.eventId == 70)
        assert(stampedSeq.last.eventId == 90)

      "Repeatedly" in:
        for _ <- 1 to (if isIntelliJIdea then 10_000 else 1000) do
          locally:
            val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=30"))
            assert(stampedSeq.head.eventId == 40)
            assert(stampedSeq.last.eventId == 60)
          locally:
            val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=60"))
            assert(stampedSeq.head.eventId == 70)
            assert(stampedSeq.last.eventId == 90)

      "/event?limit=1&after=70 rewind in last chunk" in:
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=70"))
        assert(stampedSeq.head.eventId ==  80)
        assert(stampedSeq.last.eventId == 100)

      "/event?limit=3&after=80 continue" in:
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=80"))
        assert(stampedSeq.head.eventId ==  90)
        assert(stampedSeq.last.eventId == 110)

      "/event?limit=3&after=60 rewind to oldest" in:
        val stampedSeq = getEventsByUri(Uri("/event?limit=3&after=60"))
        assert(stampedSeq.head.eventId == 70)
        assert(stampedSeq.last.eventId == 90)

      "/event?limit=3&after=150 skip some events" in:
        val runningSince = now
        val stampedSeq = getEventsByUri(Uri("/event?delay=99&limit=3&after=150"))
        assert(runningSince.elapsed < 3.s)  // Events must have been returned immediately
        assert(stampedSeq.head.eventId == 160)
        assert(stampedSeq.last.eventId == 180)

      "/event?after=180 no more events" in:
        assert(getEventsByUri(Uri("/event?timeout=0&after=180")).isEmpty)

      "/event?after=180 no more events, with timeout" in:
        val runningSince = now
        assert(getEventsByUri(Uri("/event?after=180&timeout=0.2")).isEmpty)
        assert(runningSince.elapsed >= 200.millis)
    }

    "Fetch EventIds while active is rejected" in:
      assert(eventWatch.isActiveNode)
      val response = intercept[HttpException](
        getDecodedLinesStream[EventId](Uri("/event?onlyAcks=true&timeout=0")))
      assert(response.problem == Some(AckFromActiveClusterNodeProblem))

    "Fetch EventIds while passive" in:
      val wasActive = eventWatch.isActiveNode
      eventWatch.isActiveNode = false

      assert(getDecodedLinesStream[EventId](Uri("/event?onlyAcks=true&timeout=0")) == Seq(180L))

      eventWatch.isActiveNode = wasActive

    "Fetch EventIds with heartbeat" in:
      val wasActive = eventWatch.isActiveNode
      eventWatch.isActiveNode = false

      val heartbeat = -1L
      val uri = Uri("/event?onlyAcks=true&heartbeat=0.1&timeout=3")
      val events = Stream
        .eval:
          api.getDecodedLinesStream[EventId](uri,
            returnHeartbeatAs = Some(ByteArray(heartbeat.toString)))
        .flatten
        .take(3)
        .toListL
        .await(99.s)
      assert(events == Seq(180, heartbeat, heartbeat))

      eventWatch.isActiveNode = wasActive

    //"cancel PekkoHttpClient request" in {
    //  for (i <- 1 to 16/*below Pekko's max-open-requests, see js7.conf, otherwise the pool will overflow and block*/) {
    //    logger.debug(s"cancel #$i")
    //    val future = getEvents(EventRequest.singleClass[Event](after = eventWatch.lastAddedEventId, timeout = Some(99.s)))
    //      .unsafeToFuture()
    //    sleep(10.ms)
    //    assert(!future.isCompleted)
    //    future.cancel()
    //  }
    //}

    "whenShuttingDown" - {
      "completes a running stream" in:
        val started = Promise[Unit]()
        val streamCompleted = getEventStream(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
          .onStart(IO:
            started.success(()))
          .completedL.unsafeToFuture()
        started.future await 9.s
        assert(!streamCompleted.isCompleted)
        // Shut down service
        whenShuttingDown.complete(now).await(99.s)
        streamCompleted await 99.s

      "completes a stream request, before stream started" in:
        // Shut down service, try again, in case the previous test failed
        whenShuttingDown.complete(now).await(99.s)
        val streamCompleted = getEventStream(EventRequest.singleClass[Event](after = eventWatch.lastAddedEventId, timeout = Some(99.s)))
          .completedL.unsafeToFuture()
        // Previous test has already shut down the service
        streamCompleted await 99.s
    }
  }

  private def getEventStream(eventRequest: EventRequest[Event]): Stream[IO, Stamped[KeyedEvent[Event]]] =
    getEvents(eventRequest).await(99.s)

  private def getEvents(eventRequest: EventRequest[Event]): IO[Stream[IO, Stamped[KeyedEvent[Event]]]] =
    import ControllerState.keyedEventJsonCodec
    api
      .getDecodedLinesStream[Stamped[KeyedEvent[Event]]](
        Uri("/" + encodePath("event") +
          encodeQuery(("heartbeat" -> "1"/*allows early cancellation*/) +:
            eventRequest.toQueryParameters)),
        responsive = true)

  private def getEventsByUri(uri: Uri): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    getDecodedLinesStream[Stamped[KeyedEvent[OrderEvent]]](uri)

  private def getDecodedLinesStream[A: Decoder: Tag](uri: Uri): Seq[A] =
    // TODO Without heartbeat the stream does not terminate before TCP idle timeout
    //  in about one of 1000 requests.
    //  With heartbeat the delay is until the next heartbeat.
    val heartbeatUri = Uri(s"$uri&heartbeat=1")
    Stream.eval(api.getDecodedLinesStream[A](heartbeatUri, responsive = true))
      .flatten
      .toListL
      .await(99.s)


object GenericEventRouteTest:

  private val TestEvents = for i <- 1 to 18 yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("test") ~ "VERSION"))
  private val ExtraEvent =
    Stamped(EventId(10 * 99), Timestamp.ofEpochMilli(999),
      OrderId(99.toString) <-: OrderAdded(WorkflowPath("test") ~ "VERSION"))
