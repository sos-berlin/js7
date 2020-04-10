package com.sos.jobscheduler.common.soslicense

import com.google.common.base.Splitter
import com.sos.jobscheduler.common.soslicense.LicenseKey.Parameter
import com.sos.jobscheduler.common.soslicense.LicenseKey.Parameter.{Expired, Missing, OK}
import scala.jdk.CollectionConverters._

/**
 * @author Joacim Zschimmer
 */
final case class LicenseKeyBunch(keys: Seq[LicenseKey]) extends LicenseKeyChecker {

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

  def apply(keys: String) = new LicenseKeyBunch((BunchSplitter.split(keys).asScala map { o => LicenseKey(o) }).toVector)

  def apply() = Empty
}
