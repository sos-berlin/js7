package js7.tests.controller.proxy

import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs._
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.client.AkkaHttpControllerApi
import js7.data.Problems.ItemVersionDoesNotMatchProblem
import js7.data.agent.AgentId
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.ItemOperation.{AddVersion, VersionedAddOrReplace}
import js7.data.item.VersionId
import js7.data.item.VersionedEvent.VersionedItemAdded
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.Outcome.Succeeded
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.value.NamedValues
import js7.data.workflow.position.Position
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.proxy.data.event.EventAndState
import js7.tests.controller.proxy.ClusterProxyTest.{primaryUserAndPassword, workflow}
import js7.tests.controller.proxy.JournaledProxyObservableTester.syntax._
import js7.tests.controller.proxy.JournaledProxyTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

final class JournaledProxyTest
extends AnyFreeSpec with BeforeAndAfterAll with ProvideActorSystem with ControllerAgentForScalaTest
{
  override protected def agentIds = agentId :: Nil
  protected val versionedItems = Nil
  protected def config = ProxyConfs.defaultConfig

  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateItem ]
      }
    }
    """

  private implicit def implicitActorSystem = actorSystem
  private val versionId = VersionId("MY-VERSION")
  private lazy val api = new ControllerApi(
    AkkaHttpControllerApi.resource(controller.localUri, Some(primaryUserAndPassword), name = "JournaledProxyTest") :: Nil)
  private lazy val proxy = api.startProxy().await(99.s)

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents.head.writeExecutable(RelativeExecutablePath("TEST.cmd"), script(100.ms))
    proxy
  }

  override def afterAll() = {
    proxy.stop.await(99.s)
    close()
    super.afterAll()
  }

  "updateItems" - {
    "VersionId mismatch" in {
      val response = api.updateItems(Observable(
        AddVersion(VersionId("OTHER-VERSION")),
        VersionedAddOrReplace(sign(workflow))
      )).await(99.s)
      assert(response == Left(ItemVersionDoesNotMatchProblem(VersionId("OTHER-VERSION"), workflow.id)))
    }

    "success" in {
      val myWorkflow = workflow withVersion versionId
      proxy.awaitEvent[VersionedItemAdded](_.stampedEvent.value.event.signed.value == myWorkflow) {
        api.updateItems(Observable(
          AddVersion(versionId),
          VersionedAddOrReplace(sign(myWorkflow))
        )).map { o => assert(o.orThrow == Completed) }
      }.await(99.s)
    }
  }

  "addOrders" - {
    "Adding duplicate orders is rejected" in {
      val order = FreshOrder(OrderId("DUPLICATE"), workflow.path)
      assert(api.addOrders(Observable(order, order)).await(99.s) ==
        Left(Problem("Unexpected duplicates: 2×Order:DUPLICATE")))
    }

    "success" in {
      val orderIds = (1 to 2).map(i => OrderId(s"ORDER-$i")).toSet
      val whenFinished = proxy.observable
        .collect {
          case EventAndState(Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)), _, _)
            if orderIds contains orderId =>
            orderId <-: event
        }
        .updateStateWhileInclusive(Set.empty[OrderId])(_ != orderIds) {
          case (state, KeyedEvent(orderId, _: OrderTerminated | _: OrderFailed)) =>
            state + orderId
          case (state, _) => state
        }
        .toListL
        .runToFuture
      api.addOrders(
        Observable.fromIterable(
          orderIds.map(orderId => FreshOrder(orderId, workflow.path)))
      ).await(99.s).orThrow
      val observedEvents = whenFinished.await(99.s)
      assert(observedEvents.groupMap(_.key)(_.event) ==
        orderIds
          .map(_ -> List[OrderEvent](
            OrderAdded(workflow.path ~ versionId),
            OrderAttachable(agentId),
            OrderAttached(agentId),
            OrderStarted,
            OrderProcessingStarted,
            OrderStdoutWritten("TEST ☘\n"),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1)),
            OrderDetachable,
            OrderDetached,
            OrderFinished))
          .toMap)
    }
  }
}

object JournaledProxyTest {
  private val agentId = AgentId("AGENT")
}
