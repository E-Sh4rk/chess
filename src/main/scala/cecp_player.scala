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
    private var advPlay:Option[(Int,Int,Int,Int)] = None
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
        purgeMin(in,2);purge(err)

        while (!_stop)
        {
            try
            {
                purge(in);purge(err)
                if (play)
                {
                    play = false

                    var move:Option[(Int,Int,Int,Int)] = None
                    if (advPlay != None)
                    {
                        val Some (p) = advPlay
                        advPlay = None
                        play = false
                        // TODO : Support promoting
                        sendCommand(out,game.getHistory.moveToAlgebricNotation(p))
                        purgeMin(in,3) // Time Limits + Print move
                        move = parseMove(getNextLine(in))
                        purgeMin(in,1) // My move is...
                    }

                    if (move == None)
                    {
                        sendCommand(out,"go")
                        purgeMin(in,2) // Time Limits
                        move = parseMove(getNextLine(in)) // Move.
                        purgeMin(in,1) // My move is...
                    }
                    
                    val Some ((fromX,fromY,toX,toY)) = move
                    game.move(fromX,fromY,toX,toY)
                }
                Thread.sleep(10)
            }
            catch { case e: Exception => { } }
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
    private def purgeMin(s:BufferedReader, i:Int) : Unit =
    {
        for (a <- 1 to i)
            getNextLine(s)
        purge(s)
    }
    private def getNextLine(s:BufferedReader) : String =
    {
        return s.readLine
    }
    private def parseMove(str:String) : Option[(Int,Int,Int,Int)] =
    {
        println(str);
        val algNotationMove:String = str.split(" ... ")(1)
        return game.getHistory.moveFromAlgebricNotation(algNotationMove)
    }

    def init (g:Game) : Unit =
    {
        if (game != null)
            stop
        play = false

        // Check GameMode
        if (g.getHistory.mode != GameMode.Vanilla)
            println("ERROR : gnuchess does not support non-standard game mode.")

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
    def mustPlay (advMove:Option[(Int,Int,Int,Int)]) : Unit =
    {
        if (game == null)
            return
        advPlay = advMove
        play = true
    }

}
