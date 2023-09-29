package js7.data.calendar

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class CalendarTest extends OurTestSuite:
  "JSON" in:
    testJson(
      Calendar(
        CalendarPath("CALENDAR"),
        dateOffset = 6.h,
        orderIdToDatePattern = "#([^#]+)#.*",
        periodDatePattern = "yyyy-MM-dd",
        Some(ItemRevision(1))),
      json"""
        {
          "path": "CALENDAR",
          "dateOffset": 21600,
          "orderIdPattern": "#([^#]+)#.*",
          "periodDatePattern": "yyyy-MM-dd",
          "itemRevision": 1
        } """)

    testJsonDecoder(
      Calendar(
        CalendarPath("CALENDAR"),
        orderIdToDatePattern = "#([^#]+)#.*",
        periodDatePattern = "yyyy-MM-dd"),
      json"""
        {
          "path": "CALENDAR",
          "orderIdPattern": "#([^#]+)#.*",
          "periodDatePattern": "yyyy-MM-dd"
        } """)

  "CalendarPath.itemTypeName" in:
    assert(CalendarPath.itemTypeName == Calendar.typeName)
