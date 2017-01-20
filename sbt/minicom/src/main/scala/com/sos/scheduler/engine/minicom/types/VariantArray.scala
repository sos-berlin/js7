package com.sos.scheduler.engine.minicom.types

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.collection.immutable
import scala.collection.immutable.IndexedSeq
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final case class VariantArray(indexedSeq: immutable.IndexedSeq[Any]) {

  def as[A: ClassTag]: IndexedSeq[A] = {
    require(indexedSeq forall { _.getClass isAssignableFrom implicitClass[A] },
      s"VariantArray elements not of expected type ${implicitClass[A].getName}")
    indexedSeq.asInstanceOf[immutable.IndexedSeq[A]]
  }
}
