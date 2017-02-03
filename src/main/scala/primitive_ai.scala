
class PrimitiveAI extends SynchPlayer
{
    override def synchPlay : (Int,Int,Int,Int) =
    {
        Thread.sleep(1000)
        val moves = game.possibleMoves
        val r = scala.util.Random
        return moves(r.nextInt(moves.length))
    }
}