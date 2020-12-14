package js7.tests.testenv

import js7.base.auth.{Admission, UserAndPassword}
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionToApiResource
import js7.proxy.ControllerApi

object ControllerTestUtils
{
  def newControllerApi(controller: RunningController, userAndPassword: Option[UserAndPassword] = None) =
    new ControllerApi(Seq(
      admissionToApiResource(Admission(controller.localUri, userAndPassword))(controller.actorSystem)))
}
