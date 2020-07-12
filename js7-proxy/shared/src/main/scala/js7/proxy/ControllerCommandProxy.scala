package js7.proxy

import cats.effect.Resource
import js7.base.problem.Checked
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerCommand
import monix.eval.Task

final class ControllerCommandProxy(apiResource: Resource[Task, HttpControllerApi])
{
  def execute(command: ControllerCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        api.httpClient.liftProblem(
          api.executeCommand(command))))
}
