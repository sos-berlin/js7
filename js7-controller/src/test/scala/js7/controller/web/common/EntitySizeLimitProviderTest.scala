package js7.controller.web.common

import js7.common.configutils.Configs.HoconStringInterpolator
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EntitySizeLimitProviderTest extends AnyFreeSpec
{
  "entitySizeLimit" in {
    val provider = new EntitySizeLimitProvider {
      protected def config = config"js7.web.server.services.order.post-size-limit = 50%"
      def entitySizeLimit_ = entitySizeLimit
    }
    assert(provider.entitySizeLimit_ == sys.runtime.maxMemory / 2)
  }
}
