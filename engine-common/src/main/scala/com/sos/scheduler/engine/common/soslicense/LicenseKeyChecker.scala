package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter.{Expired, Missing, OK}

/**
 * @author Joacim Zschimmer
 */
trait LicenseKeyChecker {

  final def require(parameter: Parameter, failureText: ⇒ String = ""): Unit = {
    apply(parameter) match {
      case OK ⇒ OK
      case Expired ⇒ throw new LicenseKeyParameterExpiredException(parameter, failureText = failureText)
      case Missing ⇒ throw new LicenseKeyParameterIsMissingException(parameter, failureText = failureText)
    }
  }

  def apply(parameter: Parameter): Parameter.Result
}
