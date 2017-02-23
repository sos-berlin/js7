package com.sos.jobscheduler.common.soslicense

import com.sos.jobscheduler.base.exceptions.StandardPublicException
import com.sos.jobscheduler.common.soslicense.LicenseKey.Parameter
import com.sos.jobscheduler.common.soslicense.Parameters.parameterToString

/**
 * @author Joacim Zschimmer
 */
class LicenseKeyParameterIsMissingException(parameter: Parameter, failureText: String = "")
extends StandardPublicException(s"LicenseKeyParameterIsMissingException: License key required for '${parameterToString(parameter)}'" +
  (if (failureText.isEmpty) "" else s" ($failureText)"))
