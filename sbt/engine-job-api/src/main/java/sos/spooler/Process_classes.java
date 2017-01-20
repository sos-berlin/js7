// $Id: Variable_set.java 4558 2006-10-04 13:55:00Z jz $        Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com

package sos.spooler;

/** 
 * @author Joacim Zschimmer, Zschimmer GmbH, http://www.zschimmer.com
 * @version $Revision: 4558 $
 */

public class Process_classes extends Idispatch implements HasBean<Process_classesBean>
{
    public Process_classes(Invoker invoker) {
        super(invoker);
    }

    private                 Process_classes         ( long idispatch )                  { super(idispatch); }

    public Process_class    process_class           ( String path )                     { return (Process_class)com_call( "<process_class"         , path ); }
    public Process_class    process_class_or_null   ( String path )                     { return (Process_class)com_call( "<process_class_or_null" , path ); }
    public Process_class    create_process_class    ()                                  { return (Process_class)com_call( "create_process_class"          ); }
    public void             add_process_class       ( Process_class process_class )     {                       com_call( "add_process_class"      , process_class ); }

    @Override public final Process_classesBean toBean() {
        return new Process_classesBean(this);
    }
}
