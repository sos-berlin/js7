// $Id: Spooler.java 3699 2005-06-09 14:05:10Z jz $

package sos.spooler;

import java.io.File;

/**
 *
 * @author Joacim Zschimmer, Zschimmer GmbH
 * @version $Revision: 3699 $
 */

public class Xslt_stylesheet extends Idispatch implements HasBean<Xslt_stylesheetBean>
{
    public Xslt_stylesheet(Invoker invoker) {
        super(invoker);
    }

    private                 Xslt_stylesheet     ( long idispatch )                  { super(idispatch); }

    /*+ Sollte nach dem Gebrauch gerufen werden. */
    public void             close               ()                                  { com_call( "close" ); }
    
    /*+ Lï¿½dt das Stylesheet aus einer Datei. */
    public Xslt_stylesheet  load_file           ( File file )                       { return load_file( file.toString() ); }
    
    /*+ Lï¿½dt das Stylesheet aus einer Datei. */
    public Xslt_stylesheet  load_file           ( String path )                     { return (Xslt_stylesheet)com_call( "load_file", path ); }
    
    /*+ Lï¿½dt das Stylesheet aus dem XML-String. */
    public Xslt_stylesheet  load_xml            ( String xml )                      { return (Xslt_stylesheet)com_call( "load_xml", xml ); }

    /*+ Wendet das Stylesheet auf den XML-String an.
      * @return Das transformierte XML-Dokument
      */
    public String           apply_xml           ( String xml )                      { return (String)com_call( "apply_xml", xml ); }

    @Override
    public Xslt_stylesheetBean toBean() {
        return new Xslt_stylesheetBean(this);
    }

}
