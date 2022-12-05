package js7.controller.client

import js7.base.test.OurTestSuite
import js7.base.web.Uri
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
final class ControllerUrisTest extends OurTestSuite
{
  private val controllerUris = ControllerUris(Uri("https://example.com/controller"))

  "overview" in {
    assert(controllerUris.overview == Uri("https://example.com/controller/api"))
  }

  "command" in {
    assert(controllerUris.command == Uri("https://example.com/controller/api/command"))
  }

  "order" - {
    "overview" in {
      assert(controllerUris.order.overview == Uri("https://example.com/controller/api/order"))
    }

    "single" in {
      assert(controllerUris.order(OrderId("ORDER-ID")) == Uri("https://example.com/controller/api/order/ORDER-ID"))
      assert(controllerUris.order(OrderId("/å")) == Uri("https://example.com/controller/api/order/%2F%C3%A5"))
    }
  }

  "snapshot" - {
    "list" in {
      assert(controllerUris.snapshot.list == Uri("https://example.com/controller/api/snapshot/"))
    }
  }
}
