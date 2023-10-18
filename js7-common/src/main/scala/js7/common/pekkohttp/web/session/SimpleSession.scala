package js7.common.pekkohttp.web.session

/**
  * @author Joacim Zschimmer
  */
final case class SimpleSession(sessionInit: SessionInit) extends Session:
  override def toString = sessionToken.toString
