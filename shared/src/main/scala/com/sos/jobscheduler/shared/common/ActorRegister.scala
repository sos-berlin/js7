package com.sos.jobscheduler.shared.common

import akka.actor.ActorRef
import com.sos.jobscheduler.common.scalautil.DuplicateKeyException
import java.util.NoSuchElementException
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
class ActorRegister[K, V](valueToActorRef: V ⇒ ActorRef)  {
  private val keyToValue = mutable.Map[K, V]() withDefault onUnknownKey
  private val _actorToKey = mutable.Map[ActorRef, K]()

  def onUnknownKey(k: K): Nothing =
    throw new NoSuchElementException(s"No such element '$k'")

  protected def insert(kv: (K, V)): Unit = {
    if (keyToValue contains kv._1) throw new DuplicateKeyException(s"Duplicate ${kv._1}")
    this += kv
  }

  protected def +=(kv: (K, V)): Unit = {
    keyToValue += kv
    val (k, v) = kv
    _actorToKey += valueToActorRef(v) → k
  }

  protected def -=(key: K): Unit = {
    for (v ← keyToValue.remove(key)) {
      _actorToKey -= valueToActorRef(v)
    }
  }

  protected def -=(a: ActorRef): Unit = {
    for (id ← _actorToKey.remove(a)) {
      keyToValue -= id
    }
  }

  protected def remove(key: K): Option[V] = {
    for (v ← keyToValue.remove(key)) yield {
      _actorToKey -= valueToActorRef(v)
      v
    }
  }

  final def apply(jobPath: K): V =
    keyToValue(jobPath)

  final def get(jobPath: K): Option[V] =
    keyToValue.get(jobPath)

  final def get(actorRef: ActorRef): Option[V] =
    for (k ← _actorToKey.get(actorRef);
         v ← keyToValue.get(k))
      yield v

  final def actorRefOf(key: K): ActorRef =
    valueToActorRef(keyToValue(key))

  final def actorToKey(actorRef: ActorRef): K =
    _actorToKey(actorRef)

  final def contains(key: K): Boolean =
    keyToValue isDefinedAt key

  final def contains(actorRef: ActorRef): Boolean =
    _actorToKey isDefinedAt actorRef

  final def apply(actorRef: ActorRef): V =
    keyToValue(_actorToKey(actorRef))

  override def toString = s"ActorRegister(${keyToValue.size} items)"

  final def keys: Vector[K] = keyToValue.keys.toVector

  final def values: Vector[V] = keyToValue.values.toVector

  final def isEmpty = keyToValue.isEmpty

  final def nonEmpty = keyToValue.nonEmpty

  final def size = keyToValue.size
}

object ActorRegister {
  def simple[K] = new ActorRegister[K, ActorRef](identity)
}
