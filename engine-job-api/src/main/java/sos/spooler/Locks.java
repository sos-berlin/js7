// $Id: Variable_set.java 4558 2006-10-04 13:55:00Z jz $        Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com

package sos.spooler;

/** 
 * @author Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com
 * @version $Revision: 4558 $
 */

public class Locks extends Idispatch implements HasBean<LocksBean>
{
    public Locks(Invoker invoker) {
        super(invoker);
    }

    private                 Locks               ( long idispatch )                  { super(idispatch); }

    public Lock             lock                ( String path )                     { return (Lock)         com_call( "<lock"         , path ); }

    public Lock             lock_or_null        ( String path )                     { return (Lock)         com_call( "<lock_or_null" , path ); }

    public Lock             create_lock         ()                                  { return (Lock)         com_call( "create_lock"          ); }

    public void             add_lock            ( Lock lock )                       {                       com_call( "add_lock"      , lock ); }

    @Override public final LocksBean toBean() {
        return new LocksBean(this);
    }
}
