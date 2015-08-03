package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.Parameters.parameterToString

/**
 * @author Joacim Zschimmer
 */
class LicenseKeyParameterIsMissingException(parameter: Parameter)
extends StandardPublicException(s"License key required for '${parameterToString(parameter)}'")
