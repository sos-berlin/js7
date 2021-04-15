package js7.data.item

import io.circe.Codec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.silly.SillySigner
import js7.data.item.InventoryItemTest.versionedItemJsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class InventoryItemTest extends AnyFreeSpec
{
  import InventoryItemTest.inventoryItemJsonCodec

  private val signer = SillySigner.Default
  private val itemSigner = new VersionedItemSigner(signer, versionedItemJsonCodec)

  "JSON" - {
    //"TestVersionedItem" in {
    //  val item = TestVersionedItem(TestPath("PATH") ~ "1.0", "CONTENT")
    //  val signedItem = SignedItem(item, itemSigner.sign(item))
    //  testJson[InventoryItem](signedItem,
    //    json"""{
    //      "TYPE": "SignedItem",
    //      "id": "Test:PATH~1.0",
    //      "signed": {
    //        "string": "{\"TYPE\":\"TestVersionedItem\",\"id\":{\"path\":\"PATH\",\"versionId\":\"1.0\"},\"content\":\"CONTENT\"}",
    //        "signature": {
    //          "TYPE": "Silly",
    //          "signatureString": "SILLY-SIGNATURE"
    //        }
    //      }
    //    } """)
    //}

    "TestSimpleItem" in {
      testJson[InventoryItem](TestSimpleItem(TestSimpleId("ID"), "CONTENT", Some(ItemRevision(1))),
        json"""{
          "TYPE": "TestSimpleItem",
          "content": "CONTENT",
          "id": "ID",
          "itemRevision": 1
        }""")
    }
  }
}

object InventoryItemTest
{
  private val versionedItemJsonCodec = TypedJsonCodec[VersionedItem](
    Subtype[TestVersionedItem])

  private implicit val inventoryItemJsonCodec: Codec[InventoryItem] =
    InventoryItem.jsonCodec(Seq(TestSimpleItem, TestVersionedItem))
}
