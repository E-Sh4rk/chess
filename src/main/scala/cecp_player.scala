import java.io.PrintWriter
import scala.io.Source

/**
An AI that is played by an external engine supporting the CECP protocol.

Only gnuchess is supported for now !
*/
class CECP_AI extends Player with Runnable
{
    private var game:Game = null
    private var play:Boolean = false
    private var thread:Thread = null

    override def run : Unit =
    {
        var proc:Process = null
        try
        {
            proc = Runtime.getRuntime.exec(Array("gnuchess -x"))
            val out = new PrintWriter(proc.getOutputStream)
            val err = Source.fromInputStream(proc.getErrorStream)
            val in = Source.fromInputStream(proc.getInputStream)

            // Initializing game
            out.println("new")
            // TODO : pgn load

            while (true)
            {

                if (play)
                {
                    play = false
                    
                }
            }

            out.close()
        }
        catch { case  ex : InterruptedException => { } }
        if (proc != null)
            proc.destroy
    }

    def init (g:Game) : Unit =
    {
        if (game != null)
            stop
        play = false
        game = g
        thread = new Thread(this)
    }
    def stop : Unit =
    {
        play = false
        if (thread != null){ thread.interrupt ; thread = null }
        game = null
    }
    def mustPlay : Unit =
    {
        if (game == null)
            return
        play = true
    }

}
