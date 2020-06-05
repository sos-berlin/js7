package js7.base.exceptions

trait HasIsIgnorableStackTrace
{
  /** The stack trace of some throwables is ignorable and should not pollute the log.. */
  def isIgnorableStackTrace(throwable: Throwable): Boolean =
    true
}
