package js7.proxy.javaapi.data.auth

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.auth.Admission
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.proxy.javaapi.data.common.JavaWrapper
import js7.proxy.javaapi.data.common.VavrConverters._

@javaApi
final case class JAdmission(asScala: Admission)
extends JavaWrapper
{
  protected type AsScala = Admission
}

object JAdmission
{
  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(uri: String, credentials: JCredentials) =
    new JAdmission(Admission(Uri(uri), credentials.toScala))

  @javaApi
  def checked(uri: String, credentials: JCredentials): VEither[Problem, JAdmission] =
    Uri.checked(uri)
      .map(uri => new JAdmission(Admission(uri, credentials.toScala)))
      .toVavr
}
