package sos.spooler;

import java.util.Date;

import static sos.spooler.Beans.*;

public class OrderBean implements Bean<Order> {
    private final Order delegate;

    OrderBean(Order delegate) {
        this.delegate = delegate;
    }

    public void setId(String value) {
        delegate.set_id(value);
    }

    public String getId() {
        return delegate.id();
    }

    public void setTitle(String value) {
        delegate.set_title(value);
    }

    public String getTitle() {
        return delegate.title();
    }

    public void setPriority(int value) {
        delegate.set_priority(value);
    }

    public int getPriority() {
        return delegate.priority();
    }

    public Job_chainBean getJob_chain() {
        return toBean(delegate.job_chain());
    }

    public Job_chain_nodeBean getJob_chain_node() {
        return toBean(delegate.job_chain_node());
    }

    public void setState(String value) {
        delegate.set_state(value);
    }

    public String getState() {
        return delegate.state();
    }

    public void setState_text(String value) {
        delegate.set_state_text(value);
    }

    public String getState_text() {
        return delegate.state_text();
    }

    public void setPayload(Object payload) {
        delegate.set_payload(payload);
    }

    public Object getPayload() {
        return delegate.payload();
    }

    public boolean payload_is_type(String type) {
        return delegate.payload_is_type(type);
    }

    public Run_timeBean getRun_time() {
        return toBean(delegate.run_time());
    }

    public void remove_from_job_chain() {
        delegate.remove_from_job_chain();
    }

    public String getString_next_start_time() {
        return delegate.string_next_start_time();
    }

    public void setback() {
        delegate.setback();
    }

    public String getXml() {
        return delegate.xml();
    }

    public Web_serviceBean getWeb_service() {
        return delegate.web_service().toBean();
    }

    public Web_serviceBean getWeb_service_or_null() {
        return toBean(delegate.web_service_or_null());
    }

    public Web_service_operationBean getWeb_service_operation() {
        return toBean(delegate.web_service_operation());
    }

    public Web_service_operationBean getWeb_service_operation_or_null() {
        return toBean(delegate.web_service_operation_or_null());
    }

    public void setXml_payload(String xml) {
        delegate.set_xml_payload(xml);
    }

    public String getXml_payload() {
        return delegate.xml_payload();
    }

    public void setParams(Variable_setBean v) {
        delegate.set_params(toDelegate(v));
    }

    public Variable_setBean getParams() {
        return toBean(delegate.params());
    }

    public void setAt(String dateTime) {
        delegate.set_at(dateTime);
    }

    public void setAt(Date date) {
        delegate.set_at(date);
    }

    public void setSuspended(boolean b) {
        delegate.set_suspended(b);
    }

    public boolean getSuspended() {
        return delegate.suspended();
    }

    public LogBean getLog() {
        return toBean(delegate.log());
    }

    public void setEnd_state(String value) {
        delegate.set_end_state(value);
    }

    public String getEnd_state() {
        return delegate.end_state();
    }

    public int getSetback_count() {
        return delegate.setback_count();
    }

    public void setIgnore_max_orders(boolean b) {
        delegate.set_ignore_max_orders(b);
    }

    public boolean getIgnore_max_orders() {
        return delegate.ignore_max_orders();
    }

    public String getHistory_id() {
        return delegate.history_id();
    }

    public String getLast_error() {
        return delegate.last_error();
    }

    @Override public Order getDelegate() {
        return delegate;
    }
}
