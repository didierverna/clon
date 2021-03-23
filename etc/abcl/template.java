import org.armedbear.lisp.*;

public class ~A
{
  public static void main (final String[] argv)
  {
    Runnable r = new Runnable ()
      {
	public void run()
	{
	  try
	    {
	      LispObject cmdline = Lisp.NIL;
	      for (String arg : argv)
		cmdline = new Cons (arg, cmdline);
	      cmdline.nreverse ();
	      Lisp._COMMAND_LINE_ARGUMENT_LIST_.setSymbolValue (cmdline);

	      Interpreter interpreter = Interpreter.createInstance ();
	      interpreter.eval ("(defvar extensions::*argv0* \"~A\")");
	      interpreter.eval ("(export 'extensions::*argv0* 'extensions)");

	      // This is the ASDF way, assuming that every dependency is
	      // reachable. But you may prefer or need to use Quicklisp's
	      // ql:quickload function instead.
	      interpreter.eval ("(require \"asdf\")");
	      interpreter.eval ("(asdf:load-system :net.didierverna.clon.setup)");
	      interpreter.eval ("(net.didierverna.clon.setup:configure :restricted t)");

	      Load.loadSystemFile ("/~A", false, false, false);
	    }
	  catch (ProcessingTerminated e)
	    { System.exit (e.getStatus ()); }
	}
      };

    new Thread (null, r, "interpreter", 4194304L).start();
  }
}
