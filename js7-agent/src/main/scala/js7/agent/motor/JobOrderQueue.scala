package js7.agent.motor

import cats.effect.IO
import js7.agent.motor.JobOrderQueue.*
import js7.base.metering.CallMeter
import js7.base.monixutils.SimpleLock
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.{Order, OrderId}
import scala.annotation.implicitNotFound
import scala.collection.mutable

private final class JobOrderQueue:
  private val queue = new MutableOrderQueue
  private val forceAdmissionQueue = new MutableOrderQueue
  private val lock = SimpleLock[IO]
  private val removalLock = SimpleLock[IO]

  def enqueue(orders: Seq[Order[IsFreshOrReady]]): IO[Unit] =
    lock.surround:
      IO:
        orders.foreach: order =>
          if order.forceJobAdmission then
            forceAdmissionQueue.enqueue(order)
          queue.enqueue(order)

  /** Guard a sequence of isEmpty, dequeueNextOrder and isEmpty for atomar behaviour. */
  def lockForRemoval[A](body: LockedForRemoval ?=> IO[A]): IO[A] =
    removalLock.surround:
      body(using LockedForRemoval)

  def dequeueNextOrder(onlyForcedAdmission: Boolean)(using LockedForRemoval)
  : IO[Order[IsFreshOrReady] | End] =
    lock.surround:
      IO:
        if onlyForcedAdmission then
          val order = forceAdmissionQueue.dequeueNext()
          queue.remove(order.id)
          order
        else
          val order = queue.dequeueNext()
          forceAdmissionQueue.remove(order.id)
          order

  def remove(orderId: OrderId)(using LockedForRemoval): IO[Boolean] =
    lock.surround:
      IO:
        queue.remove(orderId) && locally:
          forceAdmissionQueue.remove(orderId)
          true

  def isEmpty(onlyForcedAdmission: Boolean)(using LockedForRemoval): Boolean =
    if onlyForcedAdmission then
      forceAdmissionQueue.isEmpty
    else
      queue.isEmpty

  def isEmptyUnsafe: Boolean =
    queue.isEmpty


private object JobOrderQueue:

  private val meterRemove = CallMeter("JobOrderQueue.MutableOrderQueue.remove")

  type End = End.type
  case object End

  @implicitNotFound("Code must be wrapped in lockForRemoval")
  sealed trait LockedForRemoval
  private object LockedForRemoval extends LockedForRemoval


  private final class MutableOrderQueue:
    private val queue = mutable.ListBuffer.empty[Order[IsFreshOrReady]]
    // isQueued is for optimisation
    private val isQueued = mutable.Set.empty[OrderId]

    def enqueue(order: Order[IsFreshOrReady]): Unit =
      remove(order.id)
      queue += order
      isQueued += order.id

    def dequeueNext(): Order[IsFreshOrReady] =
      val order = queue.remove(0)
      isQueued -= order.id
      order

    // Slow
    def remove(orderId: OrderId): Boolean =
      isQueued.remove(orderId) && locally:
        meterRemove:
          queue.indexWhere(_.id == orderId) match
            case -1 => false
            case i => queue.remove(i); true

    def isEmpty: Boolean =
      queue.isEmpty

    def nonEmpty: Boolean =
      queue.nonEmpty

    override def toString =
      s"MutableOrderQueue(${queue.size} orders)" //, ${inProcess.size} in process)"
