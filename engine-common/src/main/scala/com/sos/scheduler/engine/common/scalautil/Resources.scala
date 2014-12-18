package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import com.google.common.io.{Resources => GuavaResources}
import com.google.common.base.Charsets.UTF_8

@ForCpp
object Resources {
  @ForCpp def resourceAsString(path: String) =
    GuavaResources.toString(GuavaResources.getResource(path), UTF_8)
}
