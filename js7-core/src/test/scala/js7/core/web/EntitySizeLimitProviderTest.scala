package js7.core.web

import js7.base.configutils.Configs.HoconStringInterpolator
import org.scalatest.freespec.AnyFreeSpec
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class EntitySizeLimitProviderTest extends AnyFreeSpec
{
  "entitySizeLimit" in {
    val maxMemory_ = 1000L
    val provider = new EntitySizeLimitProvider {
      override protected val maxMemory = maxMemory_
      protected def config = config"js7.web.server.services.streaming-post-size-limit-factor = 25%"
      def entitySizeLimit_ = entitySizeLimit
    }
    assert(provider.entitySizeLimit_ == maxMemory_ / 4)
  }
}
