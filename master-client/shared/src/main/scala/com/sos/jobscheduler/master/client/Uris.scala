package com.sos.jobscheduler.master.client

/**
  * @author Joacim Zschimmer
  */
trait Uris {
  def encodePath(segments: String*): String

  def encodeSegment(string: String): String

  def encodeQuery(kvs: (String, String)*): String

  def encodeQuery(kvs: Iterable[(String, String)]): String
}

object Uris extends Uris with PlatformUris/*Maybe red under IntelliJ 2016.3*/
