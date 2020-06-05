package js7.proxy

import cats.effect.Resource
import js7.base.problem.Checked
import js7.master.client.HttpMasterApi
import js7.master.data.MasterCommand
import monix.eval.Task

final class MasterCommandProxy(apiResource: Resource[Task, HttpMasterApi])
{
  def execute(command: MasterCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable(
        api.httpClient.liftProblem(
          api.executeCommand(command))))
}
