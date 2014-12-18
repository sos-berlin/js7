package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.async.StandardCallQueue._
import com.sos.scheduler.engine.common.scalautil.Logger
import scala.collection.mutable
import scala.language.existentials
import scala.util.control.NonFatal

class StandardCallQueue extends PoppableCallQueue {
  private val queue = mutable.Buffer[TimedCall[_]]()
//private val queue = mutable.UnrolledBuffer[TimedCall[_]]()    Scala 2.10.0 insert() terminiert nicht
  private var closed = false

  final def add[A](o: TimedCall[A]): Unit = {
    if (o.epochMillis == 0) logger.trace(s"Enqueue $o") else logger.debug(s"Enqueue at ${o.atString} $o")
    synchronized {
      if (closed) throw new CallQueue.ClosedException(s"CallQueue is closed. '$o' is rejected")
      val i = positionAfter(o.epochMillis)
      if (i < queue.size) queue.insert(i, o)
      else queue.append(o)  // Scala 2.10.0: queue.insert(queue.size, x) geht in eine Schleife
    }
  }

  final def close(): Unit = {
    val copy = synchronized {
      closed = true
      val result = queue.toVector
      queue.clear()
      result
    }
    for (o <- copy) {
      try o.onCancel()
      catch { case NonFatal(e) ⇒ logger.warn(s"$o.onCancel(): $e", e) }
    }
  }

  final def tryCancel[A](o: TimedCall[A]): Boolean = {
    val cancelled = synchronized {
      indexOf(o) match {
        case -1 ⇒ None
        case i ⇒ Some(queue.remove(i))
      }
    }
    for (o <- cancelled) o.onCancel()   // FIXME onCancel() wird im Thread von cancel() ausgeführt, nicht im CallRunner-Thread.
    cancelled.isDefined
  }

  final def isEmpty =
    synchronized { queue.isEmpty }

  private def indexOf[A](o: TimedCall[A]) =
    queue indexWhere { _ eq o }

  private def positionAfter(at: Long) =
    queue indexWhere { _.epochMillis > at } match {
      case -1 => queue.size
      case i => i
    }

  final def nextTime =
    headOption.fold(Long.MaxValue) { _.epochMillis }

  final def isMature =
    matureHeadOption.nonEmpty

  final def popMature(): Option[TimedCall[_]] =
    synchronized {
      matureHeadOption map { o => queue.remove(0) ensuring { _ == o } }
    }

  final def matureHeadOption =
    headOption filter timedCallIsMature

  private def headOption =
    synchronized { queue.headOption }

  private def timedCallIsMature(o: TimedCall[_]) =
    o.epochMillis <= currentTimeMillis

  override def toString =
    s"${getClass.getSimpleName} with ${queue.size} operations, next=${queue.headOption}"

  protected def currentTimeMillis = org.joda.time.DateTimeUtils.currentTimeMillis
}

object StandardCallQueue {
  private val logger = Logger(getClass)
}
