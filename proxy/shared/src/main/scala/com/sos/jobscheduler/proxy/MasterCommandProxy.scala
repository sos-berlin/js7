package com.sos.jobscheduler.proxy

import cats.effect.Resource
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand
import monix.eval.Task

final class MasterCommandProxy(apiResource: Resource[Task, HttpMasterApi])
{
  def execute(command: MasterCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable(
        api.httpClient.liftProblem(
          api.executeCommand(command))))
}
