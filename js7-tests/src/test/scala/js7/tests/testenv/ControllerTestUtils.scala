package js7.tests.testenv

import js7.base.auth.{Admission, UserAndPassword}
import js7.base.utils.CatsUtils.Nel
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResource
import js7.proxy.ControllerApi
import org.jetbrains.annotations.TestOnly

@TestOnly
object ControllerTestUtils:
  def newControllerApi(controller: TestController, userAndPassword: Option[UserAndPassword] = None) =
    new ControllerApi(
      admissionsToApiResource(Nel.one(Admission(controller.localUri, userAndPassword)))(
        controller.actorSystem))

  object syntax:
    @TestOnly
    implicit final class RichRunningController(private val controller: TestController) extends AnyVal:
      @TestOnly
      def newControllerApi(userAndPassword: Option[UserAndPassword] = None) =
        ControllerTestUtils.newControllerApi(controller, userAndPassword)
