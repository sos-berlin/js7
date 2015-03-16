// $Id: Error.java 3946 2005-09-26 08:52:01Z jz $

package sos.spooler;

/*+
 * Fehlercode und -text.
 */
/** 
 * @author Joacim Zschimmer, Zschimmer GmbH
 * @version $Revision: 3946 $
 */

public class Error extends Idispatch implements HasBean<ErrorBean>
{
    public Error(Invoker invoker) {
        super(invoker);
    }

    private                     Error                       ( long idispatch )                      { super(idispatch); }

    
    /*+ Liefert true, wenn es ein Fehler ist.
     */
    
    public boolean              is_error                    ()                                      { return boolean_com_call( "<is_error" ); }
    
    
    
    /*+ Der Fehlercode
     */
    @SchedulerGetter
    public String               code                        ()                                      { return (String)com_call( "<code"     ); }
    
    
    /*+ Der Fehlertext (mit Fehlercode)
     */
    @SchedulerGetter
    public String               text                        ()                                      { return (String)com_call( "<text"     ); }

    @Override public final ErrorBean toBean() {
        return new ErrorBean(this);
    }
}
