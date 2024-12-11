package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.SystemPropertiesExtensions.asSwitch

final class SystemPropertiesExtensionsTest extends OurTestSuite:

  "asSwitch" in:
    assert(sys.props.asSwitch("js7.SystemPropertiesExtensionsTest.undefined") == false)

    Seq(
      "" -> true,
      "false" -> false,
      "off" -> false,
      "no" -> false,
      "true" -> true,
      "on" -> true,
      "yes" -> true
    ).foreach: (string, bool) =>
      val key = "js7.SystemPropertiesExtensionsTest"
      sys.props.put(key, string)
      assert(sys.props.asSwitch(key) == bool)
