/**
A primitive AI that plays random (legal) moves.
*/
class PrimitiveAI extends SynchPlayer
{
    override def synchPlay (game:Game) : (Int,Int,Int,Int) =
    {
        Thread.sleep(500)
        val moves = game.possibleMoves.toArray
        val r = scala.util.Random
        return moves(r.nextInt(moves.length))
    }
}
