package js7.core.web

import com.typesafe.config.Config
import js7.base.convert.As.StringAsPercentage
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.scalautil.Logger
import js7.core.web.EntitySizeLimitProvider._

trait EntitySizeLimitProvider
{
  protected def config: Config

  protected lazy val streamingEntitySizeLimit = config.as("js7.web.server.services.streaming-post-size-limit-factor")(StringAsPercentage)

  protected final lazy val entitySizeLimit = {
    val limit = ((streamingEntitySizeLimit min 1.0) * sys.runtime.maxMemory).toLong
    logger.debug(s"withSizeLimit($limit)")
    limit
  }
}

object EntitySizeLimitProvider
{
  private val logger = Logger(getClass)
}
