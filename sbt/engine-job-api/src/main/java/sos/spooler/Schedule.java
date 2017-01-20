// $Id: Schedule.java 5751 2008-04-28 10:21:56Z jz $        Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com

package sos.spooler;

/** 
 * @author Joacim Zschimmer
 * @version $Revision: 5751 $
 */

public class Schedule  extends Idispatch implements HasBean<ScheduleBean>
{
    public Schedule(Invoker invoker) {
        super(invoker);
    }

    private                 Schedule            ( long idispatch )                  { super(idispatch); }

    
    
    /*+ Setzt die Run_time oder erweitert sie.
     *  
     * @param xml Ein &lt;run_time>-Dokument.
     */
    
    public void         set_xml                 ( String xml )                      {                   com_call( ">xml", xml           ); }
    @SchedulerGetter
    public String           xml                 ()                                  { return (String)   com_call( "<xml"                ); }

    @Override public final ScheduleBean toBean() {
        return new ScheduleBean(this);
    }
}
