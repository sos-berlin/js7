package js7.data_for_java.crypt

import javax.annotation.Nonnull
import js7.base.crypt.{Signed, SignedString}
import org.jetbrains.annotations.TestOnly

final case class JSigned[A](asScala: Signed[A]):
  def value: A = asScala.value
  def signedString: SignedString = asScala.signedString

  @TestOnly
  def tamper: JSigned[A] = tamperSuffix(" ")

  @TestOnly
  def tamperSuffix(suffix: String): JSigned[A] = copy(
    asScala = asScala.copy(
      signedString = asScala.signedString.copy(
        string = signedString.string + suffix)))

  @TestOnly
  def tamperSignedString(signedString: SignedString): JSigned[A] = copy(
    asScala = asScala.copy(
      signedString = signedString))


object JSigned:
  @Nonnull
  def of[A](value: A, signedString: SignedString): JSigned[A] =
    JSigned(Signed(value, signedString))
