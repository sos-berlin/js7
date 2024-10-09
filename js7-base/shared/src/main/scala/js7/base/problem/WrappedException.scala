package js7.base.problem

final class WrappedException(cause: Throwable) extends RuntimeException(cause):

  override def getMessage = cause.getMessage

  override def toString = cause.toString
