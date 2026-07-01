package js7.subagent.director.priority

import js7.data.value.NumberValue
import scala.collection.immutable.VectorBuilder
import scala.math.Ordered.orderingToOrdered

/** A sequence of elements grouped and ordered by its priority. */
private[director] final class Prioritized[A] private(
  private val orderedGroups: Vector[Vector[A]]):

  private lazy val orderedSets: Vector[Set[A]] = orderedGroups.map(_.toSet)
  private var _lastFilteredGroup = Vector.empty[A]
  private val roundRobin = MutableRoundRobin()

  /** Compares As and priority changes. */
  def isEquivalentTo(other: Prioritized[A]): Boolean =
    orderedSets == other.orderedSets

  def selectNext(filter: A => Boolean): Option[A] =
    orderedGroups.iterator
      .map: group =>
        group.filter(filter)
      .dropWhile(_.isEmpty)
      .nextOption().map: group =>
        assert(group.nonEmpty)
        roundRobin.synchronized:
          if group != _lastFilteredGroup then roundRobin.reset()
          _lastFilteredGroup = group
          val next = roundRobin.next(group.size)
          group(next)

  override def toString =
    s"Prioritized(${orderedGroups.map(_.mkString("{", " ", "}")).mkString(" ")})"


private[director] object Prioritized:

  def empty[A]: Prioritized[A] =
    new Prioritized[A](Vector.empty)

  def roundRobin[A](as: Vector[A]): Prioritized[A] =
    new Prioritized[A](Vector(as))

  def apply[A](as: Vector[(A, NumberValue)]): Prioritized[A] =
    new Prioritized[A](groupByPriority(as))

  private[subagent] def groupByPriority[A](as: Vector[(A, NumberValue)]): Vector[Vector[A]] =
    if as.isEmpty then
      Vector.empty
    else
      val ordered = as.sortWith: (x, y) =>
        x._2 > y._2

      val groups = VectorBuilder[Vector[A]]
      val group = VectorBuilder[A]()
      var i = 0
      var lastPrio = ordered.head._2: NumberValue
      while i < ordered.length do
        val (a, prio) = ordered(i)
        if prio != lastPrio then
          lastPrio = prio
          groups += group.result()
          group.clear()
        group += a
        i += 1
      if group.nonEmpty then
        groups += group.result()
      groups.result()
