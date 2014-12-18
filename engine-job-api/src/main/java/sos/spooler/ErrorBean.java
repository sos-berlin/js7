package sos.spooler;

public final class ErrorBean implements Bean<Error> {
    private final Error delegate;

    ErrorBean(Error delegate) {
        this.delegate = delegate;
    }

    public boolean isError() {
        return delegate.is_error();
    }

    public String getCode() {
        return delegate.code();
    }

    public String getText() {
        return delegate.text();
    }

    @Override public Error getDelegate() {
        return delegate;
    }
}
