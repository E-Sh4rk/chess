
/**
Represents a player (local, AI, network, etc).
*/
trait Player {

    /**
    Initializes the player.

    This function must be called when the game starts, before the first move, or when the game resumes after being suspended.
    */
    def init (game:Game) : Unit

    /**
    Indicates to the player that it is his turn.

    This function must terminate QUICLKY. Then Game.move must be called by the player (on the current thread or on another thread).
    */
    def mustPlay : Unit

    /**
    Indicates to the player that the game has been suspended.

    The move for the current round must be aborted, and every running threads of the player must be stopped.
    */
    def stop : Unit

}

/**
A simple class to extend Player which simplifies the player implementation.

You need to implement synchPlay when you extend this class. Every other method of SynchPlayer is implemented.
*/
abstract class SynchPlayer extends Player with Runnable {

    /**
    Synchronous function that is called when the player must play.
    
    It must return a (legal) move to play for the game given as parameter.
    */
    protected def synchPlay (game:Game) : (Int,Int,Int,Int)

    private var game:Game = null
    private var thread : Thread = null
    def init (g:Game) : Unit = { game = g }
    def mustPlay : Unit = { if (thread == null) {thread = new Thread(this) ; thread.start} }
    override def run : Unit =
    {
        try
        {
            val (fromX,fromY,toX,toY) = synchPlay(game)
            thread = null
            game.move(fromX,fromY,toX,toY)
        }
        catch { case  ex : InterruptedException => { } }
    }
    def stop : Unit = { if (thread != null){ thread.interrupt ; thread = null } }
}
