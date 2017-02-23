package sos.spooler;

import java.io.File;

import static sos.spooler.Beans.toBean;

/** Ein Proxy von {@link Xslt_stylesheet}, mit Gettern und Settern für Skriptsprachen. */
public class Xslt_stylesheetBean  implements Bean<Xslt_stylesheet> {
    private final Xslt_stylesheet delegate;

    Xslt_stylesheetBean(Xslt_stylesheet stylesheet) {
        delegate = stylesheet;
    }

    public void close() {
        delegate.close();
    }

    public Xslt_stylesheetBean load_file(File file) {
        return toBean(delegate.load_file(file));
    }

    public Xslt_stylesheetBean load_file(String path) {
        return toBean(delegate.load_file(path));
    }

    public Xslt_stylesheetBean load_xml(String xml) {
        return toBean(delegate.load_xml(xml));
    }

    public String apply_xml(String xml) {
        return delegate.apply_xml(xml);
    }

    @Override
    public Xslt_stylesheet getDelegate() {
        return delegate;
    }
}
