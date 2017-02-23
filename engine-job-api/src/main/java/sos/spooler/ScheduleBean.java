package sos.spooler;

public final class ScheduleBean implements Bean<Schedule> {
    private final Schedule delegate;

    ScheduleBean(Schedule delegate) {
        this.delegate = delegate;
    }

    public void setXml(String xml) {
        delegate.set_xml(xml);
    }

    public String getXml() {
        return delegate.xml();
    }

    @Override public Schedule getDelegate() {
        return delegate;
    }
}
