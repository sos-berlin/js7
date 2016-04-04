package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.convert.ConvertiblePartialFunctions
import com.sos.scheduler.engine.common.convert.ConvertiblePartialFunctions._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReaderTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ScalaXMLEventReaderTest extends FreeSpec {

  "Methods" in {
    case class X(y: Y, z: immutable.Seq[Z])
    trait T
    case class Y() extends T
    case class Z() extends T
    val x = parseString(<X><Y/><Z/><Z/></X>.toString()) { eventReader ⇒
      import eventReader._
      parseElement("X") {
        val children = forEachStartElement {
          case "Y" ⇒ parseElement() { Y() }
          case "Z" ⇒ parseElement() { Z() }
        }

        (children.values: immutable.IndexedSeq[T]) shouldEqual List(Y(), Z(), Z())

        (children.one[Y]("Y"): Y) shouldEqual Y()
        (children.one[Y]: Y)  shouldEqual Y()
        (children.option[Y]("Y"): Option[Y]) shouldEqual Some(Y())
        (children.option[Y]: Option[Y])  shouldEqual Some(Y())
        (children.byClass[Y]: immutable.Seq[Y]) shouldEqual List(Y())
        (children.byClass[Y]: immutable.Seq[Y]) shouldEqual List(Y())
        (children.byName[Y]("Y"): immutable.Seq[Y]) shouldEqual List(Y())

        intercept[IllegalArgumentException] { children.one[Z]("Z") }
        intercept[IllegalArgumentException] { children.one[Z] }
        intercept[IllegalArgumentException] { children.option[Z]("Z") }
        intercept[IllegalArgumentException] { children.option[Z] }
        (children.byClass[Z]: immutable.Seq[Z]) shouldEqual List(Z(), Z())
        (children.byClass[Z]: immutable.Seq[Z]) shouldEqual List(Z(), Z())
        (children.byName[Z]("Z"): immutable.Seq[Z]) shouldEqual List(Z(), Z())

        intercept[ClassCastException] { children.one[Y]("Z") }
        intercept[ClassCastException] { children.option[Y]("Z") }
        intercept[ClassCastException] { children.byName[Y]("Z") }

        X(children.one[Y], children.byClass[Z])
      }
    }
    x shouldEqual X(Y(), List(Z(), Z()))
  }

  "ScalaXMLEventReader" in {
    val testXmlString = <A><AA><B/><C x="xx" optional="oo"><D/><D/></C></AA></A>.toString()
    parseString(testXmlString)(parseA) shouldEqual A(B(), C(x = "xx", o = "oo", List(D(), D())))
  }

  "Whitespace and comment are ignored" in {
    val testXmlString =
      <A>
        <AA>
          <!-- comment -->
          <B/>
          <C x="xx" optional="oo"/>
          <!-- comment -->
        </AA>
      </A>.toString()
    parseString(testXmlString)(parseA) shouldEqual A(B(), C(x = "xx", o = "oo", Nil))
  }

  "Optional attribute" in {
    val testXmlString = <A><AA><B/><C x="xx"><D/><D/></C></AA></A>.toString()
    parseString(testXmlString)(parseA) shouldEqual A(B(), C(x = "xx", o = "DEFAULT", List(D(), D())))
  }

  "as and optionAs" in {
    parseString(<X int="1" empty="" wrong="xx"/>.toString()) { eventReader ⇒
      import eventReader._
      parseElement("X") {
        assertResult(1) { attributeMap.as[Int]("int") }
        assertResult(Some(1)) { attributeMap.optionAs[Int]("int") }
        assertResult(None) { attributeMap.optionAs[Int]("missing") }
        intercept[NoSuchElementException] { attributeMap.as[Int]("missing") }
        intercept[IllegalArgumentException] { attributeMap.as[Int]("empty") }
        intercept[IllegalArgumentException] { attributeMap.as[Int]("wrong") }
      }
    }
  }

//  "parseElementAsXmlString" in {
//    val testXmlString = <A><AA><B b="b">text<C/></B><B/></AA></A>.toString()
//    assertResult("""<B b="b">text<C/></B>, <B/>""") {
//      parseString(testXmlString) { eventReader ⇒
//        import eventReader._
//        parseElement("A") {
//          val children = forEachStartElement {
//            case "B" ⇒ parseElementAsXmlString()
//          }
//          children("B") mkString " ,"
//        }
//      }
//    }
//  }

  "Detects extra attribute, XmlException" in {
    val testXmlString = <A><AA><B/><C x="xx" optional="oo" z="zz"><D/><D/></C></AA></A>.toString()
    val e = intercept[XmlException] { parseString(testXmlString)(parseA) }
    e.rootCause.asInstanceOf[UnparsedAttributesException].names shouldEqual List("z")
    val XmlException(throwable) = e
    assert(throwable.getMessage == "Unknown XML attributes 'z'")
  }

  "Ignore all extra attributes" in {
    val testXmlString = <C x="xx" y="yy" z="zz"/>.toString()
    assertResult("xx") {
      parseString(testXmlString) { eventReader: ScalaXMLEventReader ⇒
        import eventReader._
        parseElement("C") {
          attributeMap.ignoreUnread()
          attributeMap("x")
        }
      }
    }
  }

  "Ignore one extra attributes" in {
    val testXmlString = <C x="xx" y="yy"/>.toString()
    assertResult("xx") {
      parseString(testXmlString) { eventReader: ScalaXMLEventReader ⇒
        import eventReader._
        parseElement("C") {
          attributeMap.ignore("y")
          attributeMap("x")
        }
      }
    }
  }

  "Detects missing attribute" in {
    val testXmlString = <A><AA><B/><C><D/><D/></C></AA></A>.toString()
    intercept[XmlException] { parseString(testXmlString)(parseA) }
      .rootCause.asInstanceOf[NoSuchElementException]
  }

  "Detects extra element" in {
    val testXmlString = <A><AA><B/><C x="xx"><D/><D/></C><EXTRA/></AA></A>.toString()
    intercept[XmlException] { parseString(testXmlString)(parseA) }
  }

  "Detects extra repeating element" in {
    val testXmlString = <A><AA><B/><C x="xx"><D/><D/><EXTRA/></C></AA></A>.toString()
    intercept[XmlException] { parseString(testXmlString)(parseA) }
  }

  "Detects missing element" in {
    val testXmlString = <A><AA><C x="xx"><D/><D/></C></AA></A>.toString()
    intercept[Exception] { parseString(testXmlString)(parseA) }
      .rootCause.asInstanceOf[NoSuchElementException]
  }

  "parseStartElementAlternative" in {
    val testXmlString = <A><Y/></A>.toString()
    parseString(testXmlString) { eventReader ⇒
      import eventReader._
      parseElement("A") {
        parseStartElementAlternative {
          case "X" ⇒ parseElement() { "XX" }
          case "Y" ⇒ parseElement() { "YY" }
          case "Z" ⇒ parseElement() { "ZZ" }
        }
      }
    } shouldEqual "YY"
  }

  "ignoreElement" in {
    val testXmlString = <A><AA><C x="xx">aa<D/>bb<D/>cc</C></AA></A>.toString()
    parseString(testXmlString) { eventReader ⇒
      eventReader.parseElement("A") {
        eventReader.ignoreElement()
      }
    }
  }

  "Exception with XML element path" in {
    val testXmlString = <A><AA><Y/></AA></A>.toString()
    intercept[XmlException] { parseString(testXmlString)(parseA) }
      .toString should include ("""XmlException: Unexpected XML element <Y> - In <A> (:1:4) <AA> (:1:8)""")
  }

  "xmlElemToStaxSource" in {
    parseElem(<A/>) { eventReader ⇒ eventReader.parseElement("A") {} }
  }
}

private object ScalaXMLEventReaderTest {
  private case class A(b: B, c: C)
  private case class B()
  private case class C(x: String, o: String, ds: immutable.Seq[D])
  private case class D()

  private def parseA(eventReader: ScalaXMLEventReader): A = {
    import eventReader._

    def parseC(): C =
      parseElement("C") {
        val x = attributeMap("x")
        val o = attributeMap.getOrElse("optional", "DEFAULT")
        val ds = parseEachRepeatingElement("D") { D() }
        C(x, o, ds.to[immutable.Seq])
      }

    parseElement("A") {
      parseElement("AA") {
        val children = forEachStartElement {
          case "B" ⇒ parseElement() { B() }
          case "C" ⇒ parseC()
        }
        A(children.one[B]("B"), children.one[C])
      }
    }
  }
}
