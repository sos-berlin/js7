package js7.core.web

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class EntitySizeLimitProviderTest extends Test
{
  "entitySizeLimit" in {
    val maxMemory_ = 1000L
    trait X {
      def entitySizeLimit_ : Long
    }
    val provider = new EntitySizeLimitProvider with X {
      override protected val maxMemory = maxMemory_
      protected def config = config"js7.web.server.services.streaming-post-size-limit-factor = 25%"
      def entitySizeLimit_ = entitySizeLimit
    }
    assert(provider.entitySizeLimit_ == maxMemory_ / 4)
  }
}
