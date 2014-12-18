package sos.spooler;

public final class Variable_setBean implements Bean<Variable_set>{
    private final Variable_set delegate;

    Variable_setBean(Variable_set delegate) {
        this.delegate = delegate;
    }

    public void set_var(String name, String value) {
        delegate.set_var(name, value);
    }

    public String var(String name) {
        return delegate.var(name);
    }

    public void set_value(String name, String value) {
        delegate.set_value(name, value);
    }

    public String value(String name) {
        return delegate.value(name);
    }

    public int getCount() {
        return delegate.count();
    }

    public void merge(Variable_setBean vars) {
        delegate.merge(vars.delegate);
    }

    public void setXml(String xmlText) {
        delegate.set_xml(xmlText);
    }

    public String getXml() {
        return delegate.xml();
    }

    public String getNames() {
        return delegate.names();
    }

    public String substitute(String string) {
        return delegate.substitute(string);
    }

    @Override public Variable_set getDelegate() {
        return delegate;
    }
}
