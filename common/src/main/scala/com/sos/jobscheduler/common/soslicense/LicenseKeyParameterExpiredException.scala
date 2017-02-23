package com.sos.jobscheduler.common.soslicense

import com.sos.jobscheduler.base.exceptions.StandardPublicException
import com.sos.jobscheduler.common.soslicense.LicenseKey.Parameter
import com.sos.jobscheduler.common.soslicense.Parameters.parameterToString

/**
 * @author Joacim Zschimmer
 */
class LicenseKeyParameterExpiredException(parameter: Parameter, failureText: String = "")
extends StandardPublicException(s"License key expired for '${parameterToString(parameter)}'" +
  (if (failureText.isEmpty) "" else s" ($failureText)"))
