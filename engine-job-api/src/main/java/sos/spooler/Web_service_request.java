// $Id: Order.java 4051 2006-01-18 19:05:50Z jz $

package sos.spooler;

/** 
 * @author Joacim Zschimmer
 * @version $Revision: 4051 $
 */



public class Web_service_request extends Idispatch implements HasBean<Web_service_requestBean>
{
    public Web_service_request(Invoker invoker) {
        super(invoker);
    }

    private                 Web_service_request     ( long idispatch )                  { super(idispatch); }

    @SchedulerGetter
    public String           url                     ()                                  { return (String)   com_call( "<Url" ); }

    public String           header                  ( String name )                     { return (String)   com_call( "<Header", name ); }

    @SchedulerGetter
    public String           content_type            ()                                  { return (String)   com_call( "<content_type" ); }

    @SchedulerGetter
    public String           charset_name            ()                                  { return (String)   com_call( "<charset_name" ); }

    @SchedulerGetter
    public String           string_content          ()                                  { return (String)   com_call( "<String_content" ); }

    @SchedulerGetter
    public byte[]           binary_content          ()                                  { return (byte[])   com_call( "<Binary_content" ); }

    @Override
    public Web_service_requestBean toBean() {
        return new Web_service_requestBean(this);
    }
}
