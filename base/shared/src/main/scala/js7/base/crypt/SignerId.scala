package js7.base.crypt

import js7.base.generic.GenericString

/**
  * Name, comment and email.
  * PGP does not seem to require uniqueness.
  * @author Joacim Zschimmer
  */
final case class SignerId(string: String) extends GenericString
