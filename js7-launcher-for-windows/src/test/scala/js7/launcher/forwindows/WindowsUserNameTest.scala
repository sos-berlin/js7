package js7.launcher.forwindows

import js7.base.test.OurTestSuite

final class WindowsUserNameTest extends OurTestSuite:
  "equals" in:
    assert(WindowsUserName("a") != WindowsUserName("b"))
    assert(WindowsUserName("a") == WindowsUserName("A"))
    assert(WindowsUserName("å") == WindowsUserName("Å"))

  "domain" in:
    assert(WindowsUserName("a").domain == None)
    assert(WindowsUserName("a@DOMAIN").domain == Some("DOMAIN"))

  "withoutDomain" in:
    assert(WindowsUserName("a").withoutDomain == "a")
    assert(WindowsUserName("a@DOMAIN").withoutDomain == "a")
