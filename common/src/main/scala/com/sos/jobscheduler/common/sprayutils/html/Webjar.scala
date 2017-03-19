package com.sos.jobscheduler.common.sprayutils.html

import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
final class Webjar(
  basePath: String,
  css: immutable.Seq[String] = Nil,
  javascripts: immutable.Seq[String] = Nil,
  val initialize: Option[String] = None)
{
  def cssPaths: immutable.Seq[String] = css map toPath

  def javascriptPaths: immutable.Seq[String] = javascripts map toPath

  private def toPath(path: String) = s"$basePath/$path"

  def allPaths: immutable.Seq[String] = (css ++ javascripts) map toPath
}
