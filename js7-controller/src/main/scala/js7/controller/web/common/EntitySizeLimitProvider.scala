package js7.controller.web.common

import com.typesafe.config.Config
import js7.base.convert.As.StringAsPercentage
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.scalautil.Logger
import js7.controller.web.common.EntitySizeLimitProvider._

trait EntitySizeLimitProvider
{
  protected def config: Config

  protected lazy val orderEntitySizeLimit = config.as("js7.web.server.services.order.post-size-limit")(StringAsPercentage)

  protected final lazy val entitySizeLimit = {
    val limit = (orderEntitySizeLimit * sys.runtime.maxMemory).toLong
    logger.debug(s"withSizeLimit($limit)")
    limit
  }
}

object EntitySizeLimitProvider
{
  private val logger = Logger(getClass)
}
