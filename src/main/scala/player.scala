
trait Player {

    def init (game:Game) : Unit

    // This function must terminate QUICLKY. It indicates that Game.move must be called (on the current thread or on another thread)
    // You can extends SynchPlayer if you want to return directly the move, regardless of the time taken.
    def mustPlay : Unit

    // Game has been suspended. The round is suspended and every running threads must be stopped.
    def stop : Unit

}

abstract class SynchPlayer extends Player with Runnable {

    protected var game:Game = null
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
