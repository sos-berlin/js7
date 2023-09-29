package js7.base.crypt

import js7.base.generic.GenericString

/**
  * Name, comment and email.
  * PGP does not seem to require uniqueness.
  * @author Joacim Zschimmer
  */
final case class SignerId private(string: String) extends GenericString

object SignerId extends GenericString.NonEmpty[SignerId]:
  override protected def unchecked(signerId: String): SignerId =
    new SignerId(signerId)

  def of(signedId: String): SignerId =
    unchecked(signedId)
