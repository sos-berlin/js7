// $Id: Job_chain.java 5753 2008-04-29 04:39:22Z jz $

package sos.spooler;

/*+
 * Eine Jobkette (Job_chain) ist eine Kette von Jobs (Jobkettenknoten oder Stellen). 
 * Diese Kette wird von Aufträgen ({@link Order}) durchlaufen.
 * <p> 
 * Jeder Stelle in der Jobkette ist ein Zustand und ein Job zugeordnet.
 * Wenn ein Auftrag der Jobkette hinzugefügt wird, 
 * setzt der Scheduler ihn an die seinem Zustand entsprechende Stelle.
 * Der dieser Stelle zugeordnete Job führt den Auftrag aus.
 * <p>
 * Jede Stelle hat außerdem einen Folgezustand und einen Fehlerzustand.
 * Nach der Verarbeitung eines Auftrags durch einen Jobschritt ändert der Scheduler den Zustand
 * des Auftrags. Wenn der Jobschritt (spooler_process) true liefert, 
 * stellt der Scheduler den Folgezustand, sonst den Fehlerzustand ein.
 * Der Auftrag rutscht damit an eine andere Stelle der Jobkette, die dem neuen Zustand zugeordnet ist.
 * 
 * <p>
 * Sie erzeugen eine Jobkette mit {@link Spooler#create_job_chain()},<br/> 
 * füllen sie mit {@link #add_job(String,String,String,String)} und {@link #add_end_state(String)}<br/>
 * und machen sie schließlich mit {@link Spooler#add_job_chain(Job_chain)} bekannt.
 * 
 * <p><br/><b>Beispiel</b>
 * <pre>
 *      Job_chain my_job_chain = spooler.create_job_chain();
 *      my_job_chain.set_name( "Jobkette" );
 *      my_job_chain.add_job( "job_100", 100,  200, 999 );
 *      my_job_chain.add_job( "job_200", 200, 1000, 999 );
 *      my_job_chain.add_end_state(  999 );
 *      my_job_chain.add_end_state( 1000 );
 *      spooler.add_job_chain( my_job_chain );
 * </pre>
 *  
 * <p><br/><b>Beispiel in JavaScript</b>
 * <pre>
 *      var my_job_chain = spooler.create_job_chain();
 *      my_job_chain.name = "Jobkette";
 *      my_job_chain.add_job( "job_100", 100,  200, 999 );
 *      my_job_chain.add_job( "job_200", 200, 1000, 999 );
 *      my_job_chain.add_end_state(  999 );
 *      my_job_chain.add_end_state( 1000 );
 *      spooler.add_job_chain( my_job_chain );
 * </pre> 
 * 
 * Jeder Knoten ist eindeutig einem Zustand zugeordnet.
 * <p>
 * Für jeden Zustand muss genau einmal add_job() oder add_end_state() aufgerufen werden. 
 *
 * @see Spooler#create_job_chain()
 * @see Spooler#job_chain(String)
 */
/** 
 * @author Joacim Zschimmer
 * @version $Revision: 5753 $
 */


public class Job_chain extends Idispatch implements HasBean<Job_chainBean>
{
    public Job_chain(Invoker invoker) {
        super(invoker);
    }

    private                 Job_chain           ( long idispatch )                  { super(idispatch); }

    
    
    /*+ Setzt den Namen der Jobkette.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     Job_chain job_chain = spooler.create_job_chain();                                                  
     *     job_chain.set_name( "Jobkette" );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     var job_chain = spooler.create_job_chain();                                                  
     *     job_chain.name = "Jobkette";
     * </pre>
     */
    public void         set_name                ( String value )                    {                           com_call( ">name", value        ); }
    
    /*+ Liefert den Namen der Jobkette.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     Job_chain job_chain = spooler.create_job_chain();                                                  
     *     job_chain.set_name( "Jobkette" );
     *     spooler_log.debug( "name=" + job_chain.name() );     // "name=Jobkette"
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     var job_chain = spooler.create_job_chain();                                                  
     *     job_chain.name = "Jobkette";
     *     spooler_log.debug( "name=" + job_chain.name );       // "name=Jobkette"
     * </pre>
     */
    @SchedulerGetter
    public String           name                ()                                  { return (String)           com_call( "<name"               ); }
    
    /*+ Fügt der Jobkette einen Knoten hinzu.
     * <p>
     * @param jobname Name des Jobs.
     * @param input_state Zustand, für den dieser Knoten gilt. Für jeden Knoten muss ein neuer Zustand angegeben sein.
     * @param output_state Zustand, den ein Auftrag nach der Ausführung des Jobschritts mit return true erhalten soll.
     * @param error_state Zustand, den ein Auftrag nach der Ausführung des Jobschritts mit return false erhalten soll.
     *   
     */   
    public void             add_job             ( String jobname, String input_state, 
                                                  String output_state, String error_state ) {                   com_call( "add_job", jobname, input_state, output_state, error_state ); }
    
    /*+ Fügt der Jobkette einen Endknoten hinzu.
     * <p/>
     * Diesem Knoten ist kein Job zugeordnet. 
     * Ein Auftrag, der einen Endknoten erreicht, hat die Jobkette durchlaufen und wird vom Scheduler aus ihr entfernt.
     * 
     */
    
    public void             add_end_state       ( String state )                    {                           com_call( "add_end_state", state ); }
    
    
    /*+ Gibt einen Auftrag in die Jobkette.
     * <p>
     * Wenn der Auftrag in einer anderen Jobkette enthalten ist,
     * entfernt der Scheduler ihn daraus.
     * <p>
     * Der Auftrag wird in die Auftragswarteschlange des Jobs, der seinem Zustand entspricht, 
     * gemäß seiner Priorität eingeordnet.
     * <p>
     * add_order() kann erst benutzt werden, wenn die Jobkette mit {@link Spooler#add_job_chain(Job_chain)} dem Scheduler übergeben worden ist.
     * <p>
     * Wenn die Jobkette bereits einen Auftrag mit gleicher Id enthält, 
     * wird eine Exception mit Fehlercode SCHEDULER-186 geliefert.
     * 
     * @see #add_or_replace_order( Order )
     * @see Spooler#create_order()
     * @see Order#remove_from_job_chain()
     */ 
    
    public void             add_order           ( Order order )                     {                           com_call( "add_order", order    ); }


    /*+ Gibt einen Auftrag in die Jobkette und ersetzt ggfs einen mit gleicher Kennung.
      * <p>
      * Hat die Jobkette bereits einen Auftrag mit gleicher Kennung, dann wird dieser ersetzt.
      * Genauer: Er wird aus der Jobkette entfernt, 
      * und der neue Auftrag wird hinzugefügt.
      * <p>
      * Solange eine Task den vorhandenen Auftrag ausführt, sind im Scheduler zwei Aufträge mit gleicher Kennung vorhanden.
      * Allerdings ist der vorhandene Auftrag aus der Jobkette und aus der Datenbank bereits entfernt.
      * Er ist nur noch für die Task nutzbar und verschwindet nach der Ausführung.
      * <p>
      * In diesem Fall wartet der Scheduler die Ausführung des neuen Auftrags ab,
      * bis die Ausführung des alten Auftrags beendet ist.
      *
      * @see #add_order(Order)
      * @see Order#remove_from_job_chain()
      */
    
    public void             add_or_replace_order( Order order )                     {                           com_call( "add_or_replace_order", order ); }
    
    
    /*+ @return Die Zahl der Aufträge in der Jobkette */
    @SchedulerGetter
    public int              order_count         ()                                  { return                int_com_call( "<order_count"        ); }
    
    /*+ @return Der Jobkettenknoten zum angegebenen Zustand */ 
    public Job_chain_node   node                ( String state )                    { return (Job_chain_node)   com_call( "<node", state        ); }
    
    /*+ Dasselbe wie node(state).job().order_queue(). 
     * @return Liefert die Auftragswarteschlange des Jobs, der dem angegebenen Zustand zugeordnet ist. */
    public Order_queue      order_queue         ( String state )                    { return (Order_queue)      com_call( "<order_queue", state ); }
    
    public void             remove              ()                                  {                           com_call( "remove" );              }
    
    public void         set_orders_recoverable  ( boolean b )                       {                           com_call( ">orders_recoverable", b ); }
    @SchedulerGetter
    public boolean          orders_recoverable  ()                                  { return            boolean_com_call( "<orders_recoverable" ); }
    
    public void         set_title               ( String title )                    {                           com_call( ">title", title ); }
    @SchedulerGetter
    public String           title               ()                                  { return (String)           com_call( "<title" ); }

    @SchedulerGetter
    public String           path                ()                                  { return (String)           com_call( "<path" ); }

    @SchedulerGetter
    public String[]         states              ()                                  { return (String[])         com_call( "<states" ); }


    public final Job_chainBean toBean() {
        return new Job_chainBean(this);
    }
}
