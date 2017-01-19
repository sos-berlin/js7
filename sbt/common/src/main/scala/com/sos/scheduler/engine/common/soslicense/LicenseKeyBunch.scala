package com.sos.scheduler.engine.common.soslicense

import com.google.common.base.Splitter
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter.{Expired, Missing, OK}
import scala.collection.JavaConversions._
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final case class LicenseKeyBunch(keys: immutable.Seq[LicenseKey]) extends LicenseKeyChecker {

  def apply(parameter: Parameter): Parameter.Result = {
    val results = keys map { _.apply(parameter) }
    if (results contains OK) OK
    else
    if (results contains Expired) Expired
    else Missing
  }

  override def toString = keys map { _.toString } mkString " "
}

object LicenseKeyBunch {
  private val BunchSplitter = Splitter.on(" ").trimResults.omitEmptyStrings
  private val Empty = new LicenseKeyBunch(Nil)

  def apply(keys: String) = new LicenseKeyBunch((BunchSplitter split keys map { o ⇒ LicenseKey(o) }).toVector)

  def apply() = Empty
}
