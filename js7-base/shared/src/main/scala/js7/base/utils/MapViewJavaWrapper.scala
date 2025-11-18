// Modified copy from original code scala.collection.convert.JavaCollectionWrappers.MapWrapper
// Adapted to MapView.
/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package js7.base.utils

import java.util as ju
import scala.collection.MapView

final class MapViewJavaWrapper[K, V](mapView: MapView[K, V]) extends ju.AbstractMap[K, V]:
  self =>

  override def size: Int =
    mapView.size

  override def get(key: AnyRef): V =
    try
      mapView.get(key.asInstanceOf[K]) match
        case None => null.asInstanceOf[V]
        case Some(v) => v
    catch
      case _: ClassCastException => null.asInstanceOf[V]

  override def entrySet: ju.Set[ju.Map.Entry[K, V]] =
    new ju.AbstractSet[ju.Map.Entry[K, V]]:
      def size = self.size

      def iterator: ju.Iterator[ju.Map.Entry[K, V]] =
        new ju.Iterator[ju.Map.Entry[K, V]]:
          val ui = mapView.iterator

          def hasNext = ui.hasNext

          def next(): ju.Map.Entry[K, V] =
            val (k, v) = ui.next()
            new ju.Map.Entry[K, V]:
              def getKey = k
              def getValue = v
              def setValue(v1 : V): V = throw new UnsupportedOperationException

              // It's important that this implementation conform to the contract
              // specified in the javadocs of java.util.Map.Entry.hashCode
              //
              // See https://github.com/scala/bug/issues/10663
              override def hashCode =
                (if k == null then 0 else k.hashCode()) ^
                (if v == null then 0 else v.hashCode())

              override def equals(other: Any) = other match
                case e: ju.Map.Entry[?, ?] => k == e.getKey && v == e.getValue
                case _ => false

  override def containsKey(key: AnyRef): Boolean =
    try
      // Note: Subclass of collection.Map with specific key type may redirect generic
      // contains to specific contains, which will throw a ClassCastException if the
      // wrong type is passed. This is why we need a type cast to A inside a try/catch.
      mapView.contains(key.asInstanceOf[K])
    catch
      case _: ClassCastException => false
