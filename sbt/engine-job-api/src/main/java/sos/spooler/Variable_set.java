// $Id: Variable_set.java 5812 2008-06-28 10:06:08Z jz $

package sos.spooler;

/*+ Variablenmenge zur Übergabe von Parametern.
 * <p> 
 * Variablenmengen werden gebraucht für die scheduler-weiten Variablen und Task-Parameter.
 * Eine neue Variablenmenge wird mit {@link Spooler#create_variable_set()} angelegt.
 * <p>
 * Die Großschreibung der Variablennamen ist relevant.
 * <p>
 * In COM (JavaScript, VBScript, Perl) ist ein Variablenwert ein Variant. 
 * Weil die Variablen in der Regel in die Scheduler-Datenbank geschrieben werden, sollten nur
 * nach String konvertierbare Variant-Werte verwendet werden (d.h. es sollten keine Objekte verwendet werden).  
 * <p>
 * In Java ist ein Variablenwert ein String. Wenn die Variable mit COM als Variant gesetzt worden ist, 
 * wird beim Lesen der nach String konvertierte Wert zurückgegeben. 
 * Null und Empty werden als null zurückgeliefert.
 * Wenn ein Variant-Wert nicht konvertierbar ist, wird eine Exception ausgelöst.
 *
 * @see Spooler#variables()
 * @see Task#params()
 * @see Spooler#create_variable_set()
 */
/** 
 * @author Joacim Zschimmer
 * @version $Revision: 5812 $
 */

public class Variable_set extends Idispatch implements HasBean<Variable_setBean>
{
    public Variable_set(Invoker invoker) {
        super(invoker);
    }

    private                 Variable_set        ( long idispatch )                  { super(idispatch); }

    /*+ Setzt eine Variable.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     Variable_set variable_set = spooler.create_variable_set();                                                  
     *     variable_set.set_var( "nachname", "Meier" );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     var variable_set = spooler.create_variable_set();                                                  
     *     variable_set( "nachname" ) = "Meier";
     * </pre>
     * 
     * 
     */ 
    public void         set_var                 ( String name, String value )       {                       com_call( ">var", name, value       ); }
    

    
    /*+ Liefert den Wert einer Variablen.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "nachname=" + spooler_task.params().var( "nachname" ) );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "nachname=" + spooler_task.params( "nachname" ) );                                                  
     * </pre>
     * 
     * @param name
     * @return Wenn die Variable nicht bekannt ist, wird "" (bei COM: ein Empty) zurückgegeben.
     */
    public String           var                 ( String name )                     { return (String)       com_call( "<var", name              ); }

    public void         set_value               ( String name, String value )       {                       com_call( ">value", name, value     ); }
    public String           value               ( String name )                     { return (String)       com_call( "<value", name            ); }

    /*+ Liefert die Anzahl der Variablen.
     * <p>
     * Es gibt keine Möglichkeit, über Variablen über einen Index anzusprechen oder über sie zu iterieren. 
     * Dieser Aufruf ist also nicht so nützlich. 
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "count=" + spooler_task.params().count() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "count=" + spooler_task.params.count );                                                  
     * </pre>
     */

    @SchedulerGetter
    public int              count               ()                                  { return            int_com_call( "<count"                  ); }
  
  //public Dom              dom                 ()
  
  
  //public Variable_set     clone               ()                                  { return (Variable_set) com_call( "clone"                   ); }
    
    
    /*+ Mischt die Variablen aus einer anderen Variablenmenge ein.
     * 
     * Bereits vorhandene Variablen werden bei gleichen Namen überschrieben.
     * 
     */
    
    public void             merge               ( Variable_set vars )               {                       com_call( "merge", vars             ); }


    /*+ Übernimmt ein die Variablenmenge aus einem XML-Dokument.
     * Mit folgender DTD:
     * <p>
     * <pre>
     *     &lt;!ELEMENT variable_set ( variable* )>
     *     &lt;!ELEMENT variable EMPTY>
     *     &lt;!ATTLIST variable name CDATA #REQUIRED>
     *     &lt;!ATTLIST variable value CDATA #REQUIRED>
     * </pre>
     *
     * Die Variablen im XML-Dokument werden dem Variable_set hinzugefügt. 
     * Vorhandene Variablen gleichen Namens werden überschrieben.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     Variable_set variable_set = spooler.create_variable_set();
     *     String xml = "&lt;?xml version='1.0'?>&lt;variable_set>&lt;variable name='nachname' value='Meier'/>&lt;variable name='vorname' value='Hans'/>&lt;/variable_set>";                                                  
     *     variable_set.set_xml( xml );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     var variable_set = spooler.create_variable_set();                                                  
     *     var xml = "&lt;?xml version='1.0'?>&lt;variable_set>&lt;variable name='nachname' value='Meier'/>&lt;variable name='vorname' value='Hans'/>&lt;/variable_set>";                                                  
     *     variable_set.xml = xml;
     * </pre>
     */

    public void         set_xml                 ( String xml_text )                 {                       com_call( ">xml", xml_text          ); }


    /*+ Liefert die Variablenmenge als XML-Dokument, wie in {@link #set_xml(String)} beschrieben.
     * Das XML-Dokument kann {@link #set_xml(String)} übergeben werden.

     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "xml=" + spooler_task.params().xml() );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "xml=" + spooler_task.params.xml );
     * </pre>
      */

    @SchedulerGetter
    public String           xml                 ()                                  { return (String)       com_call( "<xml"                    ); }

    @SchedulerGetter
    public String           names               ()                                  { return (String)       com_call( "<names"                  ); }
    
    public String           substitute          ( String string )                   { return (String)       com_call( "substitute", string      ); }

    @Override public final Variable_setBean toBean() {
        return new Variable_setBean(this);
    }
}
