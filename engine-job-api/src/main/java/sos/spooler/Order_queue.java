// $Id: Order_queue.java 3946 2005-09-26 08:52:01Z jz $

package sos.spooler;

/*+ Auftragswarteschlange eines auftragsgesteuerten Jobs.
 * 
 * <p>
 * Ein auftragsgesteuerter Job (&lt;job order="yes">) hat eine Auftragswarteschlange, 
 * die die vom Job zu verarbeitenden Aufträge aufnimmt.
 * Die Aufträge sind nach Priorität und Zeitpunkt des Eintreffens geordnet.
 * <p>
 * Verarbeiten bedeutet, dass der Scheduler die Methode {@link Job_impl#spooler_process()} einer Task des Jobs aufruft.
 * Die Methode kann über spooler_task.order() auf den Auftrag zugreifen.
 * Endet spooler_process() ohne Fehler (ohne Exception), entfernt der Scheduler den Auftrag aus der Auftragswarteschlange.
 * Ist der Auftrag in einer Jobkette, dann rückt der rückt der Auftrag an die nächste Position der Jobkette.  
 * 
 * @see Order
 * @see Job_chain
 * @see Task#order() 
 */
/** 
 * @author Joacim Zschimmer
 * @version $Revision: 3946 $
 */

public class Order_queue extends Idispatch implements HasBean<Order_queueBean>
{
    public Order_queue(Invoker invoker) {
        super(invoker);
    }

    private                 Order_queue         ( long idispatch )                  { super(idispatch); }

    /*+ Liefert die Anzahl der Aufträge in der Auftragswarteschlange. */
    @SchedulerGetter
    public int              length              ()                                  { return        int_com_call( "<length"             ); }
    
    
    
    /*+ Fügt einen Auftrag der Auftragswarteschlange hinzu.
     * <p>
     * Der Aufruf gilt nur für den Fall, dass der Auftrag nicht in einer Jobkette enthalten ist.
     * 
     * <p>
     * Die Priorität des Auftrags ({@link Order#set_priority(int)}) wird dabei berücksichtigt.
     * <p>
     * Wenn die Auftragswarteschlange bereits einen Auftrag mit gleicher Id enthält, wird dieser ersetzt.
     * 
     * @param order Der Auftrag
     * @see Job_chain#add_order(Order)
     */ 
    public Order            add_order           ( Order order )                     { return (Order)    com_call( "add_order", order    ); }

    @Override public final Order_queueBean toBean() {
        return new Order_queueBean(this);
    }
}
