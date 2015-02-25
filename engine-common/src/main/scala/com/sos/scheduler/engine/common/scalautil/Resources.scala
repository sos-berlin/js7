package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp

@ForCpp
object Resources {
  @ForCpp def resourceAsString(path: String) = JavaResource(path).asUTF8String
}
