package js7.core.web

import com.typesafe.config.Config
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.As.StringAsPercentage
import js7.base.log.Logger
import js7.base.utils.ByteUnits.toKBGB
import js7.core.web.EntitySizeLimitProvider.*

trait EntitySizeLimitProvider:
  protected def config: Config
  protected def maxMemory = sys.runtime.maxMemory

  private lazy val streamingEntitySizeLimit =
    config.as("js7.web.server.services.streaming-post-size-limit-factor")(using StringAsPercentage)

  protected final lazy val entitySizeLimit =
    val limit = ((streamingEntitySizeLimit min 1.0) * maxMemory).toLong
    logger.debug(s"HTTP POST entitySizeLimit=${toKBGB(limit)}")
    limit


object EntitySizeLimitProvider:
  private val logger = Logger[this.type]
