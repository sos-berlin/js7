package js7.common.soslicense

import js7.base.exceptions.StandardPublicException
import js7.common.soslicense.LicenseKey.Parameter
import js7.common.soslicense.Parameters.parameterToString

/**
 * @author Joacim Zschimmer
 */
class LicenseKeyParameterExpiredException(parameter: Parameter, failureText: String = "")
extends StandardPublicException(s"License key expired for '${parameterToString(parameter)}'" +
  (if (failureText.isEmpty) "" else s" ($failureText)"))
