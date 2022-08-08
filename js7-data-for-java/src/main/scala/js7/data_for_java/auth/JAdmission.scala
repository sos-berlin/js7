package js7.data_for_java.auth

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.auth.Admission
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.vavr.VavrConverters.*

@javaApi
final case class JAdmission(asScala: Admission)
extends JavaWrapper
{
  type AsScala = Admission
}

object JAdmission
{
  @javaApi @Nonnull @throws[RuntimeException]("on invalid syntax")
  def of(uri: String, credentials: JCredentials) =
    new JAdmission(Admission(Uri(uri), credentials.toScala))

  @javaApi @Nonnull
  def checked(@Nonnull uri: String, @Nonnull credentials: JCredentials): VEither[Problem, JAdmission] =
    Uri.checked(uri)
      .map(uri => new JAdmission(Admission(uri, credentials.toScala)))
      .toVavr
}
