// $Id: Order.java 6009 2009-06-12 12:08:23Z jz $

package sos.spooler;

/*+ Ein Auftrag zur Verarbeitung durch einen Job.
 * 
 * <p>
 * Ein Auftrag, der in der Auftragswarteschlange eines Jobs steht, veranlasst diesen, einen Jobschritt 
 * (also {@link Job_impl#spooler_process()}) durchzuführen.
 *
 * <p><br/><b>Beispiel</b>
 * <pre>
 *     Order order = spooler.create_order();
 *     order.set_id   ( "10001" );
 *     order.set_title( "Auftrag für Datensatz 10001" );
 *     order.set_state( "100" );
 *     spooler.job_chain( "jobkette1" ).add_order( order );
 * </pre>
 *
 * <p><br/><b>Beispiel (Javascript)</b>
 * <pre>
 *     var order = spooler.create_order();
 *     order.id    = "10001";
 *     order.title = "Auftrag für Datensatz 10001";
 *     order.state = 100;
 *     spooler.job_chain( "jobkette1" ).add_order( order );
 * </pre>
 *
 * @see Spooler#create_order()
 * @see Spooler#job_chain(String)
 * @see Task#order() 
 */

import java.util.Date;

/**
 * @author Joacim Zschimmer
 * @version $Revision: 6009 $
 */



public class Order extends Idispatch implements HasBean<OrderBean>
{
    public Order(Invoker invoker) {
        super(invoker);
    }

    private                 Order               ( long idispatch )                  { super(idispatch); }

    
    
    /*+ Stellt die Kennung des Auftrags ein.
     * 
     * <p>
     * Jeder Auftrag hat eine (innerhalb der Jobkette oder der Auftragswarteschlange des Jobs eindeutige) Kennung.
     * Diese Kennung sollten den zu verarbeitenden Daten entsprechen.
     * Übrlicherweise wird der Schlüssel eines Datenbanksatzes verwendet.
     * <p>
     * Ohne diesen Aufruf vergibt der Aufruf {@link Job_chain#add_order(Order)} bzw. {@link Order_queue#add_order(Order)} eine Kennung.
     *  
     * @param value Die Kennung
     */
    
    public void         set_id                  ( String value )                    {                   com_call( ">id", value          ); }
    
    
    
    /*+ Liefert die Kennung des Auftrags. */
    @SchedulerGetter
    public String           id                  ()                                  { return            com_call( "<id"                 ).toString(); }
    
    
    
    /*+ Der Titel ist ein Klartext, der den Auftrag bezeichnet. */ 
    public void         set_title               ( String value )                    {                   com_call( ">title", value       ); }

    
    
    /*+ Der Titel ist ein Klartext, der den Auftrag bezeichnet. */
    @SchedulerGetter
    public String           title               ()                                  { return (String)   com_call( "<title"              ); }
    
    
    
    /*+ Aufträge mit höherer Priorität werden zuerst verarbeitet. */
    public void         set_priority            ( int value )                       {                   com_call( ">priority", value    ); }
    

    
    /*+ Aufträge mit höherer Priorität werden zuerst abgearbeitet. */
    @SchedulerGetter
    public int              priority            ()                                  { return        int_com_call( "<priority"           ); }
    
    
    
    /*+ Liefert die Jobkette, in der der Auftrag enthalten ist, oder null. */
    @SchedulerGetter
    public Job_chain        job_chain           ()                                  { return (Job_chain)com_call( "<job_chain"          ); }

    
    
    /*+ Liefert den Jobkettenknoten, der dem Zustand des Auftrags entspricht, oder null, wenn der Auftrag nicht in einer Jobkette ist. */
    @SchedulerGetter
    public Job_chain_node   job_chain_node      ()                                  { return (Job_chain_node)com_call( "<job_chain_node" ); }

    
    /*+ Stellt den Zustand des Auftrags auf den Zustand ein, für den in der Jobkette der angegebene Job eingestellt ist.
     * 
     * <p>
     * Besser ist der Aufruf von {@link #set_state(String)}.
     */
  //public void         set_job                 ( Job job )                         {                   com_call( ">job", job           ); }

    
    
    /*+ Stellt den Zustand des Auftrags auf den Zustand ein, der dem Job in Jobkette entspricht.
     * 
     * <p>
     * Besser ist der Aufruf von {@link #set_state(String)}.
     */
  //public void         set_job                 ( String job_name )                 {                   com_call( ">job", job_name      ); }
    
    
    
    /*+
     * Liefert den Job, in dessen Auftragswarteschlange sich der Auftrag befindet, oder null.
     */
  //public Job              job                 ()                                  { return (Job)      com_call( "<job"                ); }
    
    
    
    /*+
     * Stellt den Zustand des Auftrags ein.
     * 
     * <p>
     * Wenn der Auftrag in einer Jobkette ist, wird der Auftrag an die entsprechende Stelle der Jobkette verschoben.
     * @param value
     */
    public void         set_state               ( String value )                    {                   com_call( ">state", value       ); }
    
    
    
    /*+
     * Liefert den Zustand des Auftrags.
     */
    @SchedulerGetter
    public String           state               ()                                  { return (String)   com_call( "<state"              ); }

    
    
    /*+ Stellt einen Klartext ein, der den Zustand des Auftrags beschreibet. */
    public void         set_state_text          ( String value )                    {                   com_call( ">state_text", value  ); }
    
    
    
    /*+ Liefert den mit set_state_text() eingestellten Text. */
    @SchedulerGetter
    public String           state_text          ()                                  { return (String)   com_call( "<state_text"         ); }

    
    
    /*+ Die Nutzlast, also Parameter des Auftrags.
     * Neben der Auftragskennung (id), die den Auftrag identifiziert, können hier zusätzliche
     * Angaben gemacht werden. 
     * 
     * @param payload Ein String oder ein {@link Variable_set}.
     * 
     * @see #set_id(String)
     * @see Spooler#create_variable_set()
     */
    public void         set_payload             ( Object payload )                  {                   com_call( ">payload", payload   ); }

    
    
    /*+ Liefert den mit set_payload() eingestellten Wert. 
      * Das kann ein {@link Variable_set} sein: 
      * <pre>
      *     Variable_set variable_set = (Variable_set)spooler_task.payload();
      *     String       value        = variable_set.var( "einparameter" );
      * </pre>
    */
    @SchedulerGetter
    public Object           payload             ()                                  { return            com_call( "<payload"            ); }
    
    
    
    /*+ Prüft den COM-Typ der Nutzlast.
     * 
     * @param name "Spooler.Variable_set", "Hostware.Dyn_obj" oder "Hostware.Record".  
     * @return true, wenn die Nutzlast vom angegebenen COM-Typ ist. 
     */
    public boolean          payload_is_type     ( String type )                     { return    boolean_com_call( "payload_is_type", type ); }


    /*+ Liefert die &lt;run_time> (zur periodischen Wiederholung des Auftrags).
     * 
     */
    @SchedulerGetter
    public Run_time         run_time            ()                                  { return (Run_time) com_call( "<run_time"           ); }


    /*+ Entfernt den Auftrag aus seiner Jobkette.
      * <p>
      * Wenn der Auftrag gerade von einer Task ausgeführt wird,
      * liefert die Eigenschaft {@link #job_chain}
      * weiterhin die Jobkette, aus der der Auftrag gerade entfernt wird.
      * Erst wenn die Ausführung beendet ist, liefert die Eigenschaft null
      * (außer der Auftrag ist wieder in eine Jobkette eingetragen worden).
      * Damit bleibt die Eigenschaft job_chain während der Ausführung durch die Task stabil.
      */
    public void             remove_from_job_chain()                                 {                   com_call( "remove_from_job_chain" ); }
    

  //(Konvertierung nach Date macht Probleme wegen Sommerzeit)
  //public Date             at                  ()                                  { return (Date)     com_call( "<at"                 ); }



    /*+ Liefert "yyyy-mm-dd HH:MM:SS.MMM" oder "now" oder "never" */
    @SchedulerGetter
    public String           string_next_start_time()                                { return (String)   com_call( "<string_next_start_time" ); }
    
    
    public void             setback             ()                                  {                   com_call( "setback" ); }

    @SchedulerGetter
    public String           xml                 ()                                  { return (String)   com_call( "<xml" ); }

    @SchedulerGetter
    public Web_service      web_service         ()                                  { return (Web_service)com_call( "<web_service" ); }

    @SchedulerGetter
    public Web_service      web_service_or_null ()                                  { return (Web_service)com_call( "<web_service_or_null" ); }

    @SchedulerGetter
    public Web_service_operation web_service_operation()                            { return (Web_service_operation)com_call( "<web_service_operation" ); }

    @SchedulerGetter
    public Web_service_operation web_service_operation_or_null ()                   { return (Web_service_operation)com_call( "<web_service_operation_or_null" ); }

    public void         set_xml_payload         ( String xml )                      {                   com_call( ">xml_payload", xml   ); }
    @SchedulerGetter
    public String           xml_payload         ()                                  { return (String)   com_call( "<xml_payload"        ); }
    
    public void         set_params              ( Variable_set v )                  {                      com_call( ">params", v ); }
    @SchedulerGetter
    public Variable_set     params              ()                                  { return (Variable_set)com_call( "<params" ); }
    
    public void         set_at                  ( String date_time )                { com_call( ">at", date_time ); }
  //public void         set_at                  ( java.util.Date date )             { com_call( ">at", date ); }
    public void         set_at                  ( Date date )                       { set_at( new java.text.SimpleDateFormat( "yyyy-MM-dd HH:mm:ss.SSS" ).format( date ) ); }
    
    public void         set_suspended           ( boolean b )                       {                        com_call( ">suspended", b ); }
    @SchedulerGetter
    public boolean          suspended           ()                                  { return         boolean_com_call( "<suspended" ); }

    @SchedulerGetter
    public Log              log                 ()                                  { return (Log)           com_call( "<log" ); }

    public void         set_end_state           ( String value )                    {                   com_call( ">end_state", value  ); }
    @SchedulerGetter
    public String           end_state           ()                                  { return (String)   com_call( "<end_state"         ); }

    @SchedulerGetter
    public int              setback_count       ()                                  { return ((Integer) com_call( "<setback_count"     )).intValue(); }

    public void         set_ignore_max_orders   ( boolean b )                       {                   com_call( ">ignore_max_orders", b ); }
    @SchedulerGetter
    public boolean          ignore_max_orders   ()                                  { return    boolean_com_call( "<ignore_max_orders" ); }

    @SchedulerGetter
    public String history_id() {
        return (String)com_call("<history_id");
    }

    @SchedulerGetter
    public String last_error() {
        return (String)com_call("<last_error");
    }

    public final OrderBean toBean() {
        return new OrderBean(this);
    }
}
