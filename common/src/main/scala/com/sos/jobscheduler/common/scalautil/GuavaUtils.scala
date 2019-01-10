package com.sos.jobscheduler.common.scalautil

import com.google.common.io.CharSource
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8

/**
  * @author Joacim Zschimmer
  */
object GuavaUtils
{
  def stringToInputStream(string: String): InputStream =
    CharSource.wrap(string).asByteSource(UTF_8).openStream()
}
