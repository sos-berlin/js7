// $Id: Run_time.java 5854 2008-08-14 08:22:14Z jz $

package sos.spooler;

/*+ &lt;run_time>-Element.
 * 
 * @see Order#run_time()
 */
/** 
 * @author Joacim Zschimmer
 * @version $Revision: 5854 $
 */

public class Run_time extends Idispatch implements HasBean<Run_timeBean>
{
    public Run_time(Invoker invoker) {
        super(invoker);
    }

    private                 Run_time            ( long idispatch )                  { super(idispatch); }

    
    
    /*+ Setzt die Run_time oder erweitert sie.
     *  
     * @param xml Ein &lt;run_time>-Dokument.
     */
    
    public void         set_xml                 ( String xml )                      {                   com_call( ">xml", xml           ); }
  //public String           xml                 ()                                  { return (String)   com_call( "<xml"                ); }

  @SchedulerGetter
    public Schedule     schedule                ()                                  { return (Schedule) com_call( "<schedule"           ); }

    @Override public final Run_timeBean toBean() {
        return new Run_timeBean(this);
    }
}
