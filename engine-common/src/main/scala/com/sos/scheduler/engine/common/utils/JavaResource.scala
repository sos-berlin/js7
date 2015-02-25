package com.sos.scheduler.engine.common.utils

import com.google.common.base.Charsets._
import com.google.common.io.Resources
import com.google.common.io.Resources.getResource
import java.io.File
import java.net.URL

/**
 * @author Joacim Zschimmer
 */
final case class JavaResource(path: String) {
  require(!(path startsWith "/"), s"JavaResource must not start with a slash: $path")

  def requireExistence() = {
    url
    this
  }

  def asUTF8String = Resources.toString(url, UTF_8)

  def simpleName = new File(path).getName

  lazy val url: URL = getResource(path)
}

object JavaResource {
  def apply(o: Package) = new JavaResource(o.getName.replace('.', '/'))
}
