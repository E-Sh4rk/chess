/**
A player that uses a preprogrammed list of moves.
*/
class SimulatedPlayer(private val moves:History) extends Player
{
    private var game:Game = null
    private var nextMoveIndex = 0

    def init (g:Game) : Unit = { game = g }
    def mustPlay : Unit = { }
    def stop : Unit = { game = null }

    /**
    Plays the next move.

    Returns true if there still are new moves to play in the list, and false otherwise.
    */
    def playNextMove () : Boolean =
    {
        if (game != null)
        {
            if (nextMoveIndex >= moves.moves.length)
            return false
        
            val m = moves.moves(nextMoveIndex)
            if (game.canMove(m.fromX,m.fromY,m.toX,m.toY))
            {
                game.move(m.fromX,m.fromY,m.toX,m.toY)
                nextMoveIndex += 1
            }
        }
        return nextMoveIndex >= moves.moves.length
    }
}
