package js7.core.common

import akka.actor.ActorRef
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.DuplicateKeyException
import js7.base.utils.StackTraces.*
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
class ActorRegister[K, V](valueToActorRef: V => ActorRef):
  private val keyToValue = mutable.Map.empty[K, V]
    .withDefault(k => throw noSuchKeyProblem(k).throwable.appendCurrentStackTrace)
  private val _actorToKey = mutable.Map.empty[ActorRef, K]

  protected def noSuchKeyProblem(k: K): Problem = Problem(s"No such key: $k")

  protected def insert(kv: (K, V)): Unit =
    if keyToValue contains kv._1 then throw new DuplicateKeyException(s"Duplicate ${kv._1}, existing: ${keyToValue(kv._1)}")
    this += kv

  protected def update(kv: (K, V)): Unit =
    require(valueToActorRef(keyToValue(kv._1)) == valueToActorRef(kv._2), "ActorRef must not change")
    keyToValue(kv._1) = kv._2

  protected def +=(kv: (K, V)): Unit =
    keyToValue += kv
    val (k, v) = kv
    _actorToKey += valueToActorRef(v) -> k

  protected def -=(key: K): Unit =
    for v <- keyToValue.remove(key) do
      _actorToKey -= valueToActorRef(v)

  protected def -=(a: ActorRef): Unit =
    for id <- _actorToKey.remove(a) do
      keyToValue -= id

  protected def remove(key: K): Option[V] =
    for v <- keyToValue.remove(key) yield
      _actorToKey -= valueToActorRef(v)
      v

  final def apply(key: K): V =
    keyToValue(key)

  final def apply(actorRef: ActorRef): V =
    keyToValue(_actorToKey(actorRef))

  final def checked(key: K): Checked[V] =
    keyToValue.get(key) toChecked noSuchKeyProblem(key)

  final def get(key: K): Option[V] =
    keyToValue.get(key)

  final def get(actorRef: ActorRef): Option[V] =
    for k <- _actorToKey.get(actorRef);
         v <- keyToValue.get(k)
      yield v

  final def actorRefOf(key: K): ActorRef =
    valueToActorRef(keyToValue(key))

  final def actorToKey(actorRef: ActorRef): K =
    _actorToKey(actorRef)

  final def contains(key: K): Boolean =
    keyToValue isDefinedAt key

  final def contains(actorRef: ActorRef): Boolean =
    _actorToKey isDefinedAt actorRef

  override def toString = s"ActorRegister(${keyToValue.size} items)"

  final def keys: Vector[K] = keyToValue.keys.toVector

  final def values: Vector[V] = keyToValue.values.toVector

  final def isEmpty = keyToValue.isEmpty

  final def nonEmpty = keyToValue.nonEmpty

  final def size = keyToValue.size

object ActorRegister:
  def simple[K] = new ActorRegister[K, ActorRef](identity)
