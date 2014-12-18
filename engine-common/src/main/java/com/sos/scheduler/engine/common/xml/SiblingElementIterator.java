package com.sos.scheduler.engine.common.xml;

import com.google.common.collect.AbstractIterator;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import javax.annotation.Nullable;

import static org.w3c.dom.Node.ELEMENT_NODE;

public class SiblingElementIterator extends AbstractIterator<Element> {
    @Nullable private Element next;

    public SiblingElementIterator(@Nullable Node firstSibling) {
        this.next = sameOrNextElement(firstSibling);
    }

    @Override protected final Element computeNext() {
        Element result = next;
        if (next != null)
            next = sameOrNextElement(next.getNextSibling());
        return result == null? endOfData() : result;
    }

    @Nullable private static Element sameOrNextElement(@Nullable Node node) {
        Node n = node;
        while (n != null && n.getNodeType() != ELEMENT_NODE)
            n = n.getNextSibling();
        return (Element)n;
    }
}