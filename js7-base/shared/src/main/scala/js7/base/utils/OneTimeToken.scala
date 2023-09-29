package js7.base.utils

import js7.base.generic.GenericString

final case class OneTimeToken(string: String) extends GenericString

object OneTimeToken extends GenericString.Checked_[OneTimeToken]:
  protected def unchecked(string: String) =
    new OneTimeToken(string)

  def random(): OneTimeToken =
    OneTimeToken(Base64UUID.randomString())
