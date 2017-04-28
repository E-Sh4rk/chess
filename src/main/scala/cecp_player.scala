import java.io.PrintWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.Source

/**
An AI that is played by an external engine supporting the CECP protocol.

Only gnuchess is supported for now !
*/
class CECP_AI extends Player with Runnable
{
    private var game:Game = null
    private var _stop = false
    private var play:Boolean = false
    private var thread:Thread = null

    override def run : Unit =
    {
        var proc = Runtime.getRuntime.exec(Array("gnuchess", "-x"))
        val out = new PrintWriter(proc.getOutputStream)
        val err = new BufferedReader(new InputStreamReader(proc.getErrorStream))
        val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

        // Initializing game
        sendCommand(out,"new")
        // TODO : pgn load
        purge(in);purge(err)

        while (!_stop)
        {
            if (play)
            {
                play = false
                purge(in)
                sendCommand(out,"go")
                getNextLine(in) ; getNextLine(in) // TimeLimits
                println(getNextLine(in)) // Move. TODO : Parse and send
            }
            Thread.sleep(10)
        }

        out.close()
        if (proc != null)
            proc.destroy
        _stop = false
    }
    private def sendCommand(out:PrintWriter,s:String) : Unit =
    {
        out.println(s)
        out.flush
    }
    private def purge(s:BufferedReader) : Unit =
    {
        while (s.ready)
            s.read
    }
    private def getNextLine(s:BufferedReader) : String =
    {
        return s.readLine
    }

    def init (g:Game) : Unit =
    {
        if (game != null)
            stop
        play = false
        game = g
        thread = new Thread(this)
        thread.start
    }
    def stop : Unit =
    {
        play = false
        if (thread != null)
        {
            _stop = true
            thread = null
        }
        game = null
    }
    def mustPlay : Unit =
    {
        if (game == null)
            return
        play = true
    }

}
