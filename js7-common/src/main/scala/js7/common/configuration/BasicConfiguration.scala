package js7.common.configuration

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.common.commandline.CommandLineArguments

trait BasicConfiguration:
  def config: Config
  def maybeConfigDirectory: Option[Path] = None
  def maybeDataDirectory: Option[Path] = None


object BasicConfiguration:

  trait Companion[Cnf <: BasicConfiguration]:

    given Companion[Cnf] = this

    final def args(args: String*): Cnf =
      CommandLineArguments.parse(args)(fromCommandLine)

    def fromCommandLine(args: CommandLineArguments): Cnf


  type Empty = Empty.type
  object Empty extends BasicConfiguration with Companion[Empty]:
    val config = ConfigFactory.empty()

    def fromCommandLine(args: CommandLineArguments): Empty =
      args.requireNoMoreArguments()
      Empty
