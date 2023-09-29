package js7.common.configuration

import com.typesafe.config.Config
import java.nio.file.Path

trait BasicConfiguration:
  def config: Config
  def maybeConfigDirectory: Option[Path] = None
  def maybeDataDirectory: Option[Path] = None
