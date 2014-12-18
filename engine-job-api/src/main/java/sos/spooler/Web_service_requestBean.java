package sos.spooler;

/** Ein Proxy von {@link Web_service_request}, mit Gettern und Settern für Skriptsprachen. */
public class Web_service_requestBean implements Bean<Web_service_request> {
    private final Web_service_request delegate;

    Web_service_requestBean(Web_service_request web_service_request) {
        delegate = web_service_request;
    }

    public String getUrl() {
        return delegate.url();
    }

    public String header(String name) {
        return delegate.header(name);
    }

    public String getContent_type() {
        return delegate.content_type();
    }

    public String getCharset_name() {
        return delegate.charset_name();
    }

    public String getString_content() {
        return delegate.string_content();
    }

    public byte[] getBinary_content() {
        return delegate.binary_content();
    }

    @Override
    public Web_service_request getDelegate() {
        return delegate;
    }
}
