// $Id: Variable_set.java 4558 2006-10-04 13:55:00Z jz $        Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com

package sos.spooler;

/** 
 * @author Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com
 * @version $Revision: 4558 $
 */

public class Lock extends Idispatch implements HasBean<LockBean>
{
    public Lock(Invoker invoker) {
        super(invoker);
    }

    private                 Lock                ( long idispatch )                  { super(idispatch); }

    public void         set_name                ( String name )                     {                       com_call( ">name"             , name ); }

    @SchedulerGetter
    public String           name                ()                                  { return (String)       com_call( "<name"                    ); }

    public void         set_max_non_exclusive   ( int n )                           {                       com_call( ">max_non_exclusive", n    ); }

    @SchedulerGetter
    public int              max_non_exclusive   ()                                  { return            int_com_call( "<max_non_exclusive"       ); }

    public void             remove              ()                                  {                       com_call( "remove"                   ); }

    @Override public LockBean toBean() {
        return new LockBean(this);
    }
}
