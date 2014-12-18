package com.sos.scheduler.engine.common.xml;

import com.google.common.collect.ImmutableList;
import org.junit.Test;
import org.w3c.dom.Element;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;

public final class SiblingElementIteratorTest {
    @Test public void test1() {
        Element e1 = XmlUtils.loadXml("<root><a/><b/><a/><b/><a/></root>").getDocumentElement();
        Element e2 = XmlUtils.loadXml("<root> <a/> <b/> <a/> <b/> <a/> </root>").getDocumentElement();
        for (Element element: new Element[]{e1, e2}) {
            assertThat(ImmutableList.copyOf(new SiblingElementIterator(element.getFirstChild())), hasSize(5));
        }
    }
}
