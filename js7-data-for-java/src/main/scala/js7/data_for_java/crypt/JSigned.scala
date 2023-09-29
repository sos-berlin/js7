package js7.data_for_java.crypt

import javax.annotation.Nonnull
import js7.base.crypt.{Signed, SignedString}

final case class JSigned[A](asScala: Signed[A]):
  def value = asScala.value
  def signedString = asScala.signedString

  def tamper = tamperSuffix(" ")

  def tamperSuffix(suffix: String) = copy(
    asScala = asScala.copy(
      signedString = asScala.signedString.copy(
        string = signedString.string + suffix)))

  def tamperSignedString(signedString: SignedString) = copy(
    asScala = asScala.copy(
      signedString = signedString))

object JSigned:
  @Nonnull
  def of[A](value: A, signedString: SignedString) =
    JSigned(Signed(value, signedString))
