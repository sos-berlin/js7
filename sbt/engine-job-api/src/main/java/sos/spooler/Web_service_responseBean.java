package sos.spooler;

/** Ein Proxy von {@link Web_service_response}, mit Gettern und Settern für Skriptsprachen. */
public class Web_service_responseBean implements Bean<Web_service_response> {
    private final Web_service_response delegate;

    Web_service_responseBean(Web_service_response web_service_response) {
        delegate = web_service_response;
    }

    public String header(String name) {
        return delegate.header(name);
    }

    public void setStatus_code(int code) {
        delegate.set_status_code(code);
    }

    public void set_header(String name, String value) {
        delegate.set_header(name, value);
    }

    public void setContent_type(String content_type) {
        delegate.set_content_type(content_type);
    }

    public String getContent_type() {
        return delegate.content_type();
    }

    public void setCharset_name(String charset_name) {
        delegate.set_charset_name(charset_name);
    }

    public String getCharset_name() {
        return delegate.charset_name();
    }

    public void setString_content(String content) {
        delegate.set_string_content(content);
    }

    public void setBinary_content(byte[] content) {
        delegate.set_binary_content(content);
    }

    public void send() {
        delegate.send();
    }

    @Override
    public Web_service_response getDelegate() {
        return delegate;
    }
}
