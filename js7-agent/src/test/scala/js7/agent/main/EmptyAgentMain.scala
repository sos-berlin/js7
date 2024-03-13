package js7.agent.main

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.catsutils.OurApp

/** For testing only.
  * @author Joacim Zschimmer
  */
object EmptyAgentMain extends OurApp:

  private given IORuntime = runtime

  def run(args: List[String]): IO[ExitCode] =
    val resource = for
      provider <- TestAgentDirectoryProvider.resource[IO]
      conf = AgentConfiguration.forTest(
        configAndData = provider.agentDirectory,
        name = AgentConfiguration.DefaultName,
        httpPort = Some(4445))
      agent <- TestAgent.resource(conf)
    yield
      agent

    resource.use:
      _.untilTerminated.map(_.toExitCode)
