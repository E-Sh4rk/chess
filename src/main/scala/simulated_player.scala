/**
A player that uses a preprogrammed list of moves.
*/
class SimulatedPlayer(private val moves:/*History*/Int) extends Player
{
    private var game:Game = null

    def init (g:Game) : Unit = { game = g }
    def mustPlay : Unit = { }
    def stop : Unit = { game = null }

    /**
    Plays the next move.

    Returns true if there still are new moves to play in the list, and false otherwise.
    */
    def playNextMove () : Boolean =
    {
        // TODO
        return false
    }
}
