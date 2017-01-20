package sos.spooler;

import java.io.File;

import static sos.spooler.Beans.toBean;

public final class LogBean implements Bean<Log> {
    private final Log delegate;

    LogBean(Log delegate) {
        this.delegate = delegate;
    }

    public void error(String line) {
        delegate.error(line);
    }

    public void warn(String line) {
        delegate.warn(line);
    }

    public void info(String line) {
        delegate.info(line);
    }

    public void debug(String line) {
        delegate.debug(line);
    }

    public void debug1(String line) {
        delegate.debug1(line);
    }

    public void debug2(String line) {
        delegate.debug2(line);
    }

    public void debug3(String line) {
        delegate.debug3(line);
    }

    public void debug4(String line) {
        delegate.debug4(line);
    }

    public void debug5(String line) {
        delegate.debug5(line);
    }

    public void debug6(String line) {
        delegate.debug6(line);
    }

    public void debug7(String line) {
        delegate.debug7(line);
    }

    public void debug8(String line) {
        delegate.debug8(line);
    }

    public void debug9(String line) {
        delegate.debug9(line);
    }

    public void log(int level, String line) {
        delegate.log(level, line);
    }

    public void log_file(String path) {
        delegate.log_file(path);
    }

    public void log_file(File file) {
        delegate.log_file(file);
    }

    public MailBean getMail() {
        return toBean(delegate.mail());
    }

    public void setMail_on_warning(boolean value) {
        delegate.set_mail_on_warning(value);
    }

    public boolean getMail_on_warning() {
        return delegate.mail_on_warning();
    }

    public void setMail_on_error(boolean value) {
        delegate.set_mail_on_error(value);
    }

    public boolean getMail_on_error() {
        return delegate.mail_on_error();
    }

    public void setMail_on_success(boolean value) {
        delegate.set_mail_on_success(value);
    }

    public boolean getMail_on_success() {
        return delegate.mail_on_success();
    }

    public void setMail_on_process(int steps) {
        delegate.set_mail_on_process(steps);
    }

    public int getMail_on_process() {
        return delegate.mail_on_process();
    }

    public void setLevel(int level) {
        delegate.set_level(level);
    }

    public int getLevel() {
        return delegate.level();
    }

    public String getFilename() {
        return delegate.filename();
    }

    public void setNew_filename(String filename) {
        delegate.set_new_filename(filename);
    }

    public String getNew_filename() {
        return delegate.new_filename();
    }

    public void start_new_file() {
        delegate.start_new_file();
    }

    public void setMail_it(boolean mailIt) {
        delegate.set_mail_it(mailIt);
    }

    public String getLast_error_line() {
        return delegate.last_error_line();
    }

    public String last(String level) {
        return delegate.last(level);
    }

    public String last(int level) {
        return delegate.last(level);
    }

    @Override public Log getDelegate() {
        return delegate;
    }
}
