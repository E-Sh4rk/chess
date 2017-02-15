
/**
Represents a player (local, AI, network, etc).
*/
trait Player {

    /**
    Initialize the player.

    This function must be called when the game starts, before the first move, or when the game resume after being suspended.
    */
    def init (game:Game) : Unit

    /**
    Indicate to the player that it's his turn.

    This function must terminate QUICLKY. Then, Game.move must be called by the player (on the current thread or on another thread).
    */
    def mustPlay : Unit

    /**
    Indicate to the player that the game has been suspended.

    The move for the current round must be aborted, and every running threads of the player must be stopped.
    */
    def stop : Unit

}

/**
A simple class to extends that simplify the player implementation.

You need to implement synchPlay when you extend this class. Every other method of player is implemented.
*/
abstract class SynchPlayer extends Player with Runnable {

    protected var game:Game = null
    /**
    Synchronous function that is called when the player must play.
    
    It must return a (legal) move to play. The current game can be accessed through the variable "game".
    */
    protected def synchPlay : (Int,Int,Int,Int)

    private var thread : Thread = null
    def init (g:Game) : Unit = { game = g }
    def mustPlay : Unit = { if (thread == null) {thread = new Thread(this) ; thread.start} }
    override def run : Unit =
    {
        try
        {
            val (fromX,fromY,toX,toY) = synchPlay
            thread = null
            game.move(fromX,fromY,toX,toY)
        }
        catch { case  ex : InterruptedException => { } }
    }
    def stop : Unit = { if (thread != null){ thread.interrupt ; thread = null } }
}
