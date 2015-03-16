// $Id: Order.java 4051 2006-01-18 19:05:50Z jz $

package sos.spooler;

/** 
 * @author Joacim Zschimmer
 * @version $Revision: 4051 $
 */



public class Web_service_operation extends Idispatch implements HasBean<Web_service_operationBean>
{
    public Web_service_operation(Invoker invoker) {
        super(invoker);
    }

    private                     Web_service_operation   ( long idispatch )          { super(idispatch); }


    @SchedulerGetter
    public Web_service          web_service             ()                          { return (Web_service         )   com_call( "<web_service" ); }

    @SchedulerGetter
    public Web_service_request  request                 ()                          { return (Web_service_request )   com_call( "<request"     ); }

    @SchedulerGetter
    public Web_service_response response                ()                          { return (Web_service_response)   com_call( "<response"    ); }

    @SchedulerGetter
    public String               peer_hostname           ()                          { return (String)                 com_call( "<peer_hostname" ); }

    @SchedulerGetter
    public String               peer_ip                 ()                          { return (String)                 com_call( "<peer_ip" ); }

    @Override
    public Web_service_operationBean toBean() {
        return new Web_service_operationBean(this);
    }
}
