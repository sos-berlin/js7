package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions._
import com.sos.scheduler.engine.base.utils.ScalaUtils._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReaderTest._
import com.sos.scheduler.engine.common.scalautil.xmls.XmlSources._
import com.sos.scheduler.engine.common.time.Stopwatch.measureTime
import javax.xml.transform.Source
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
    val x = parseDocument(<X><Y/><Z/><Z/></X>) { eventReader ⇒
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
    val testXmlString = <A><AA><B/><C x="xx" optional="oo"><D/><D/></C></AA></A>
    parseDocument(testXmlString)(parseA) shouldEqual A(B(), C(x = "xx", o = "oo", List(D(), D())))
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
      </A>
    parseDocument(testXmlString)(parseA) shouldEqual A(B(), C(x = "xx", o = "oo", Nil))
    parseDocument(testXmlString)(_.ignoreElements())
  }

  "matchElement" in {
    def parse(source: Source) = parseDocument(source) { eventReader ⇒
      eventReader.matchElement {
        case "A" ⇒ fail()
        case "B" ⇒ eventReader.parseElement() { "OK" }
      }
    }
    assert(parse(<B/>) == "OK")
    intercept[IllegalArgumentException] { parse(<X/>) }
  }

  "Optional attribute" in {
    val testXmlString = <A><AA><B/><C x="xx"><D/><D/></C></AA></A>
    parseDocument(testXmlString)(parseA) shouldEqual A(B(), C(x = "xx", o = "DEFAULT", List(D(), D())))
  }

  "as and optionAs" in {
    parseDocument(<X int="1" empty="" wrong="xx"/>) { eventReader ⇒
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
//      parseDocument(testXmlString) { eventReader ⇒
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
    val testXmlString = <A><AA><B/><C x="xx" optional="oo" z="zz"><D/><D/></C></AA></A>
    val e = intercept[XmlException] { parseDocument(testXmlString)(parseA) }
    e.rootCause.asInstanceOf[UnparsedAttributesException].names shouldEqual List("z")
    val XmlException(throwable) = e
    assert(throwable.getMessage == "Unknown XML attributes 'z'")
  }

  "Ignore all extra attributes" in {
    val testXmlString = <C x="xx" y="yy" z="zz"/>
    assertResult("xx") {
      parseDocument(testXmlString) { eventReader: ScalaXMLEventReader ⇒
        import eventReader._
        parseElement("C") {
          attributeMap.ignoreUnread()
          attributeMap("x")
        }
      }
    }
  }

  "Ignore one extra attributes" in {
    val testXmlString = <C x="xx" y="yy"/>
    assertResult("xx") {
      parseDocument(testXmlString) { eventReader: ScalaXMLEventReader ⇒
        import eventReader._
        parseElement("C") {
          attributeMap.ignore("y")
          attributeMap("x")
        }
      }
    }
  }

  "Detects missing attribute" in {
    val testXmlString = <A><AA><B/><C><D/><D/></C></AA></A>
    intercept[XmlException] { parseDocument(testXmlString)(parseA) }
      .rootCause.asInstanceOf[NoSuchElementException]
  }

  "Detects extra element" in {
    val testXmlString = <A><AA><B/><C x="xx"><D/><D/></C><EXTRA/></AA></A>
    intercept[XmlException] { parseDocument(testXmlString)(parseA) }
  }

  "Detects extra repeating element" in {
    val testXmlString = <A><AA><B/><C x="xx"><D/><D/><EXTRA/></C></AA></A>
    intercept[XmlException] { parseDocument(testXmlString)(parseA) }
  }

  "Detects missing element" in {
    val testXmlString = <A><AA><C x="xx"><D/><D/></C></AA></A>
    intercept[Exception] { parseDocument(testXmlString)(parseA) }
      .rootCause.asInstanceOf[NoSuchElementException]
  }

  "parseStartElementAlternative" in {
    val testXmlString = <A><Y/></A>
    parseDocument(testXmlString) { eventReader ⇒
      import eventReader._
      parseElement("A") {
        parseStartElementAlternative {
          case "X" ⇒ Some(parseElement() { "XX" })
          case "Y" ⇒ Some(parseElement() { "YY" })
          case "Z" ⇒ Some(parseElement() { "ZZ" })
          case _ ⇒ None
        }
      }
    } shouldEqual Some("YY")
  }

  "ignoreElement" in {
    val testXmlString = <A><AA><C x="xx">aa<D/>bb<D/><!--COMMENT-->cc</C></AA></A>
    parseDocument(testXmlString) { eventReader ⇒
      eventReader.parseElement("A") {
        eventReader.ignoreElement()
      }
    }
  }

  "Exception with XML element path" in {
    val testXmlString = <A><AA><Y/></AA></A>
    intercept[XmlException] { parseDocument(testXmlString)(parseA) }
      .toString should include ("""XmlException: Unexpected XML element <Y> - In <A> (:1:4) <AA> (:1:8)""")
  }

  "xmlElemToStaxSource" in {
    parseDocument(<A/>) { eventReader ⇒ eventReader.parseElement("A") {} }
  }

  if (sys.props contains "test.speed") "Speed for minimal XML document" in {
    for (_ ← 1 to 10) measureTime(10000, "document") {
      parseDocument("<A/>") { eventReader ⇒
        eventReader.parseElement("A") {}
      }
    }
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
        C(x, o, ds)
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
