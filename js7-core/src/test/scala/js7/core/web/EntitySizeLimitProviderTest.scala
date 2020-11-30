package js7.core.web

import js7.common.configutils.Configs.HoconStringInterpolator
import org.scalatest.freespec.AnyFreeSpec
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class EntitySizeLimitProviderTest extends AnyFreeSpec
{
  "entitySizeLimit" in {
    val provider = new EntitySizeLimitProvider {
      protected def config = config"js7.web.server.services.streaming-post-size-limit-factor = 25%"
      def entitySizeLimit_ = entitySizeLimit
    }
    assert(provider.entitySizeLimit_ == sys.runtime.maxMemory / 4)
  }
}
