package com.sos.scheduler.engine.http.client.common

import spray.http.{HttpHeader, Rendering}

/**
  * @author Joacim Zschimmer
  */
abstract class OwnHttpHeader extends HttpHeader {
  def companion: OwnHttpHeaderCompanion
  def name = companion.name
  def lowercaseName = companion.lowercaseName
  def render[R <: Rendering](r: R): r.type = r ~~ name ~~ ':' ~~ ' ' ~~ value
}
