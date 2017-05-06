import java.io.PrintWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths

/**
An AI that is played by an external engine supporting the CECP protocol.

Only gnuchess is supported for now !
*/
class CECP_AI extends Player with Runnable
{
    private var game:Game = null
    private var _stop = false
    private var play:Boolean = false
    private var advMove:Move = null
    private var thread:Thread = null
    private var proc:Process = null

    override def run : Unit =
    {
        // Clean former gnuchess cache file (may cause issues)
        var temp = Paths.get(".tmp.epd")
        Files.deleteIfExists(temp)

        proc = Runtime.getRuntime.exec(Array("gnuchess", "-x"))
        val out = new PrintWriter(proc.getOutputStream)
        val err = new BufferedReader(new InputStreamReader(proc.getErrorStream))
        val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

        // Initializing game
        purgeMin(in,1);purge(err)
        sendCommand(out,"new")
        // PGN Load
        if (game.getHistory.moves.length > 0)
        {
            purge(in);purge(err)
            temp = Files.createTempFile("gnuchess", ".pgn")
            game.getHistory.savePGN(temp.toString, true)
            sendCommand(out,"pgnload "+temp.toString)
            purgeMin(in,10);purge(err)
            Files.delete(temp);
        }

        while (!_stop)
        {
            try
            {
                purge(in);purge(err)
                if (play)
                {
                    play = false
                    var move:Move = null

                    if (advMove != null)
                    {
                        sendCommand(out,game.getHistory.moveToAlgebricNotation(advMove))
                        advMove = null
                        purgeMin(in,1) // Print move
                        move = parseMove(getNextLine(in))
                        purgeMin(in,1) // My move is...
                    }

                    if (move == null)
                    {
                        sendCommand(out,"go")
                        move = parseMove(getNextLine(in)) // Move.
                        purgeMin(in,1) // My move is...
                    }
                    
                    game.move(move.fromX,move.fromY,move.toX,move.toY,move.promotion)
                }
                Thread.sleep(10)
            }
            catch { case e: Exception => { } }
        }

        out.close()
        if (proc != null)
            proc.destroy
        proc = null
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
        var str = s.readLine
        while (str.startsWith("TimeLimit") || str.isEmpty)
            str = s.readLine
        return str
    }
    private def parseMove(str:String) : Move =
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
        game = null
        if (thread != null)
        {
            _stop = true
            thread = null
        }
        if (proc != null)
            proc.destroy
        proc = null
    }
    def mustPlay (_advMove:Move) : Unit =
    {
        if (game == null)
            return
        advMove = _advMove
        play = true
    }

}
