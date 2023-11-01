package js7.tests.testenv

import js7.base.auth.{Admission, UserAndPassword}
import js7.base.utils.CatsUtils.Nel
import js7.controller.RunningController
import js7.controller.client.PekkoHttpControllerApi.admissionToApiResource
import js7.proxy.ControllerApi
import org.jetbrains.annotations.TestOnly

object ControllerTestUtils
{
  @TestOnly
  def newControllerApi(controller: RunningController, userAndPassword: Option[UserAndPassword] = None) =
    new ControllerApi(Nel.one(
      admissionToApiResource(Admission(controller.localUri, userAndPassword))(controller.actorSystem)))

  object syntax
  {
    implicit final class RichRunningController(private val controller: RunningController) extends AnyVal
    {
      @TestOnly
      def newControllerApi(userAndPassword: Option[UserAndPassword] = None) =
        ControllerTestUtils.newControllerApi(controller, userAndPassword)
    }
  }
}
