// $Id: Variable_set.java 4558 2006-10-04 13:55:00Z jz $        Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com

package sos.spooler;

/** 
 * @author Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com
 * @version $Revision: 4558 $
 */

public class Process_class extends Idispatch implements HasBean<Process_classBean>
{
    public Process_class(Invoker invoker) {
        super(invoker);
    }

    private                 Process_class       ( long idispatch )                  { super(idispatch); }

    public void         set_name                ( String name )                     {                       com_call( ">name"             , name ); }
    @SchedulerGetter
    public String           name                ()                                  { return (String)       com_call( "<name"                    ); }

    public void         set_remote_scheduler    ( String host_and_port )            {                       com_call( ">remote_scheduler" , host_and_port ); }
    @SchedulerGetter
    public String           remote_scheduler    ()                                  { return (String)       com_call( "<remote_scheduler"        ); }

    public void         set_max_processes       ( int n )                           {                       com_call( ">max_processes"    , n    ); }
    @SchedulerGetter
    public int              max_processes       ()                                  { return            int_com_call( "<max_processes"           ); }

    public void             remove              ()                                  {                       com_call( "remove"                   ); }

    @Override public final Process_classBean toBean() {
        return new Process_classBean(this);
    }
}
