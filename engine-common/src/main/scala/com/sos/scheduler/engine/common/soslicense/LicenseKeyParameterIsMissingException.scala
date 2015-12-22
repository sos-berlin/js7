package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.Parameters.parameterToString

/**
 * @author Joacim Zschimmer
 */
class LicenseKeyParameterIsMissingException(parameter: Parameter, failureText: String = "")
extends StandardPublicException(s"LicenseKeyParameterIsMissingException: License key required for '${parameterToString(parameter)}'" +
  (if (failureText.isEmpty) "" else s" ($failureText)"))
