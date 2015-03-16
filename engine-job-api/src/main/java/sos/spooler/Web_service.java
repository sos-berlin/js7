// $Id: Order.java 4051 2006-01-18 19:05:50Z jz $

package sos.spooler;

/** 
 * @author Joacim Zschimmer
 * @version $Revision: 4051 $
 */



public class Web_service extends Idispatch implements HasBean<Web_serviceBean>
{
    public Web_service(Invoker invoker) {
        super(invoker);
    }

    private                 Web_service         ( long idispatch )                  { super(idispatch); }


    @SchedulerGetter
    public String           name                ()                                  { return (String)   com_call( "<name" ); }

    @SchedulerGetter
    public String           forward_xslt_stylesheet_path()                          { return (String)   com_call( "<forward_xslt_stylesheet_path" ); }

    @SchedulerGetter
    public Variable_set     params              ()                                  { return (Variable_set)com_call( "<params" ); }

    @Override
    public Web_serviceBean toBean() {
        return new Web_serviceBean(this);
    }
}
