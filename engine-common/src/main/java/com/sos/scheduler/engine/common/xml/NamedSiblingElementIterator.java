package com.sos.scheduler.engine.common.xml;

import com.google.common.collect.AbstractIterator;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import javax.annotation.Nullable;

import static org.w3c.dom.Node.ELEMENT_NODE;

public class NamedSiblingElementIterator extends AbstractIterator<Element> {
    private final String name;
    @Nullable private Element next;

    public NamedSiblingElementIterator(String elementName, Node firstSibling) {
        name = elementName;
        next = sameOrNextElement(firstSibling);
    }

    @Override protected final Element computeNext() {
        Element result = next;
        if (next != null)
            next = sameOrNextElement(next.getNextSibling());
        return result == null? endOfData() : result;
    }

    @Nullable protected final Element sameOrNextElement(@Nullable Node node) {
        Node n = node;
        while (n != null && (n.getNodeType() != ELEMENT_NODE || !n.getNodeName().equals(name)))
            n = n.getNextSibling();
        return (Element)n;
    }
}