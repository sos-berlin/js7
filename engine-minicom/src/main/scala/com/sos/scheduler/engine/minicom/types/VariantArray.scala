package com.sos.scheduler.engine.minicom.types

import com.sos.scheduler.engine.base.utils.ScalaUtils
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.collection.immutable
import scala.reflect.ClassTag


/**
 * @author Joacim Zschimmer
 */
final case class VariantArray(indexedSeq: immutable.IndexedSeq[Any]) {
  def as[A : ClassTag] = {
    require(indexedSeq forall { _.getClass isAssignableFrom implicitClass[A] })
    indexedSeq.asInstanceOf[immutable.IndexedSeq[A]]
  }
}
