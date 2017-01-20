package com.sos.scheduler.engine.taskserver.spoolerapi

/**
 * @author Joacim Zschimmer
 */
final class UnsupportedApiException(classAndMethodName: String) extends UnsupportedOperationException {
  override def getMessage = s"Universal Agent does not support method '$classAndMethodName'"
}
