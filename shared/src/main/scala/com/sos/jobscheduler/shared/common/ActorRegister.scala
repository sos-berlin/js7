package com.sos.jobscheduler.shared.common

import akka.actor.ActorRef
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class ActorRegister[K, V](valueToActorRef: V ⇒ ActorRef)  {
  private val keyToValue = mutable.Map[K, V]()
  private val _actorToKey = mutable.Map[ActorRef, K]()

  def +=(kv: (K, V)): Unit = {
    keyToValue += kv
    val (k, v) = kv
    _actorToKey += valueToActorRef(v) → k
  }

  def -=(key: K): Unit = {
    for (v ← keyToValue.remove(key)) {
      _actorToKey -= valueToActorRef(v)
    }
  }

  def remove(key: K): Option[V] = {
    for (v ← keyToValue.remove(key)) yield {
      _actorToKey -= valueToActorRef(v)
      v
    }
  }

  def -=(a: ActorRef): Unit = {
    for (id ← _actorToKey.remove(a)) {
      keyToValue -= id
    }
  }

  def apply(jobPath: K): V =
    keyToValue(jobPath)

  def get(jobPath: K): Option[V] =
    keyToValue.get(jobPath)

  def get(actorRef: ActorRef): Option[V] =
    for (k ← _actorToKey.get(actorRef);
         v ← keyToValue.get(k))
      yield v

  def getOrElseUpdate(key: K, update: ⇒ V): V =
    get(key) match {
      case Some(v) ⇒ v
      case None ⇒
        val value = update
        this += key → value
        value
    }

  def actorRefOf(key: K): ActorRef =
    valueToActorRef(keyToValue(key))

  def actorToKey(actorRef: ActorRef): K =
    _actorToKey(actorRef)

  def isDefinedAt(key: K): Boolean =
    keyToValue isDefinedAt key

  def isDefinedAt(actorRef: ActorRef): Boolean =
    _actorToKey isDefinedAt actorRef

  def apply(actorRef: ActorRef): V =
    keyToValue(_actorToKey(actorRef))

  override def toString = s"ActorRegister(${keyToValue.size} items)"

  def keys: Vector[K] = keyToValue.keys.toVector

  def values: Vector[V] = keyToValue.values.toVector

  def isEmpty = keyToValue.isEmpty

  def nonEmpty = keyToValue.nonEmpty

  def contains(k: K) = keyToValue contains k

  def size = keyToValue.size
}

object ActorRegister {
  def simple[K] = new ActorRegister[K, ActorRef](identity)
}
