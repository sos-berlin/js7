package sos.spooler;

import static sos.spooler.Beans.toBean;

/** Ein Proxy von {@link Web_service}, mit Gettern und Settern für Skriptsprachen. */
public class Web_serviceBean implements Bean<Web_service> {
    private final Web_service delegate;

    Web_serviceBean(Web_service web_service) {
        delegate = web_service;
    }

    public String getName() {
        return delegate.name();
    }

    public String getForward_xslt_stylesheet_path() {
        return delegate.forward_xslt_stylesheet_path();
    }

    public Variable_setBean getParams() {
        return toBean(delegate.params());
    }

    @Override
    public Web_service getDelegate() {
        return delegate;
    }
}
