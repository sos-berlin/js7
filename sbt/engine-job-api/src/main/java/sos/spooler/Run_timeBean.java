package sos.spooler;

import static sos.spooler.Beans.toBean;

public final class Run_timeBean implements Bean<Run_time> {
    private final Run_time delegate;

    Run_timeBean(Run_time delegate) {
        this.delegate = delegate;
    }

    public void setXml(String xml) {
        delegate.set_xml(xml);
    }

    public ScheduleBean getSchedule() {
        return toBean(delegate.schedule());
    }

    @Override public Run_time getDelegate() {
        return delegate;
    }
}
