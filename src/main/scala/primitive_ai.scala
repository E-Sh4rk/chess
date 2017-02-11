/**
A primitive AI that play random (legal) moves.
*/
class PrimitiveAI extends SynchPlayer
{
    override def synchPlay : (Int,Int,Int,Int) =
    {
        Thread.sleep(500)
        val moves = game.possibleMoves
        val r = scala.util.Random
        return moves(r.nextInt(moves.length))
    }
}