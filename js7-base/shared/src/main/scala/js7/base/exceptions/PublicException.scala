package js7.base.exceptions

/**
 * Exception.getMessage is allowed to be visible by the user (via a web browser).
 *
 * @author Joacim Zschimmer
 */
trait PublicException:
  this: Throwable =>

  def publicMessage: String = 
    Option(getMessage) getOrElse getClass.getName
