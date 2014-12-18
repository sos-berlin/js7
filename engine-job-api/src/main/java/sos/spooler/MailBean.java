package sos.spooler;

import java.io.File;

public final class MailBean implements Bean<Mail> {
    private final Mail delegate;

    MailBean(Mail delegate) {
        this.delegate = delegate;
    }

    public void setTo(String recipients) {
        delegate.set_to(recipients);
    }

    public String getTo() {
        return delegate.to();
    }

    public void setFrom(String from) {
        delegate.set_from(from);
    }

    public String getFrom() {
        return delegate.from();
    }

    public void setCc(String recipients) {
        delegate.set_cc(recipients);
    }

    public String getCc() {
        return delegate.cc();
    }

    public void setBcc(String recipients) {
        delegate.set_bcc(recipients);
    }

    public String getBcc() {
        return delegate.bcc();
    }

    public void setSubject(String text) {
        delegate.set_subject(text);
    }

    public String getSubject() {
        return delegate.subject();
    }

    public void setBody(String text) {
        delegate.set_body(text);
    }

    public String getBody() {
        return delegate.body();
    }

    public void add_file(String pathName, String mailFilename, String contentType, String encoding) {
        delegate.add_file(pathName, mailFilename, contentType, encoding);
    }

    public void add_file(String realFilename, String mailFilename, String contentType) {
        delegate.add_file(realFilename, mailFilename, contentType);
    }

    public void add_file(String realFilename, String mailFilename) {
        delegate.add_file(realFilename, mailFilename);
    }

    public void add_file(String realFilename) {
        delegate.add_file(realFilename);
    }

    public void setSmtp(String hostname) {
        delegate.set_smtp(hostname);
    }

    public String getSmtp() {
        return delegate.smtp();
    }

    public void setQueue_dir(String directory) {
        delegate.set_queue_dir(directory);
    }

    public String getQueue_dir() {
        return delegate.queue_dir();
    }

    public void add_header_field(String fieldName, String value) {
        delegate.add_header_field(fieldName, value);
    }

    public int dequeue() {
        return delegate.dequeue();
    }

    public String getDequeue_log() {
        return delegate.dequeue_log();
    }

    public void setXslt_stylesheet_path(String path) {
        delegate.set_xslt_stylesheet_path(path);
    }

    public void setXslt_stylesheet_path(File file) {
        delegate.set_xslt_stylesheet_path(file);
    }

    public String getXslt_stylesheet_path() {
        return delegate.xslt_stylesheet_path();
    }

    @Override public Mail getDelegate() {
        return delegate;
    }
}
