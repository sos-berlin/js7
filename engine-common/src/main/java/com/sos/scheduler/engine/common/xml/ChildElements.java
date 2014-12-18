package com.sos.scheduler.engine.common.xml;

import org.w3c.dom.Element;

import java.util.Iterator;

public class ChildElements implements Iterable<Element> {
    private final Element parent;

    public ChildElements(Element parent) {
        this.parent = parent;
    }

    @Override public final Iterator<Element> iterator() {
        return new SiblingElementIterator(parent.getFirstChild());
    }
}
