package js7.base.exceptions

/**
 * @author Joacim Zschimmer
 */
class StandardPublicException(override val publicMessage: String, cause: Throwable | Null = null)
extends RuntimeException(publicMessage, cause), PublicException:

  def this(publicMessage: String) = this(publicMessage, null: Throwable | Null)
