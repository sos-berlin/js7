package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class PlannableBoardTest extends OurTestSuite:

  "JSON" in:
    given Codec[PlannableBoard] = TypedJsonCodec[PlannableBoard](PlannableBoard.subtype)
    testJson(
      PlannableBoard(BoardPath("BOARD"), Some(ItemRevision(7))),
      json"""{
        "TYPE": "PlannableBoard",
        "path": "BOARD",
        "itemRevision": 7
      }""")

    testJsonDecoder(
      PlannableBoard(BoardPath("BOARD")),
      json"""{
        "TYPE": "PlannableBoard",
        "path": "BOARD"
      }""")

  "BoardPath.itemTypeName" in:
    assert(BoardPath.itemTypeName == "Board")
    assert(PlannableBoard.typeName == "PlannableBoard")
