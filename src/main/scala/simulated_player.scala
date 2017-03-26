/**
A player that uses a preprogrammed list of moves.
*/
class SimulatedPlayer(private val moves:History) extends Player
{
    private var game:Game = null
    private var nextMoveIndex = 0
    private var autoPlayUntil = -1

    def init (g:Game) : Unit = { game = g }
    def stop : Unit = { game = null }
    def mustPlay : Unit =
    {
        if (nextMoveIndex <= autoPlayUntil)
            playNextMove
    }

    /**
    Plays the next move.

    Returns true if there still are new moves to play in the list, and false otherwise.
    */
    def playNextMove () : Boolean =
    {
        if (game != null)
        {
            if (nextMoveIndex >= moves.moves.length)
            {
                autoPlayUntil = -1
                return false
            }
        
            val m = moves.moves(nextMoveIndex)

            if (m.castle != CastleType.NoCastle)
            {
                // Castle move !
                val (k_x,k_y) = game.getKing(game.getRound).getPosition
                m.fromX = k_x ; m.fromY = k_y
                val k_lside =  2 * k_x / game.dim_x == 0
                val k_go_lside = if (m.castle == CastleType.Kingside) k_lside else !k_lside
                val to_x = if (k_go_lside) k_x - 2 else k_x + 2
                m.toX = to_x ; m.toY = k_y
            }

            if (m.fromX < 0 || m.fromY < 0)
            {
                // We try to guess the move
                val pieces = game.getPieces(game.getRound,m.pieceType)
                for (p <- pieces)
                {
                    val (p_x,p_y) = p.getPosition
                    if ( (p_x == m.fromX || m.fromX < 0) && (p_y == m.fromY || m.fromY < 0) )
                        if (game.canMove(p_x,p_y,m.toX,m.toY))
                        { m.fromX = p_x ; m.fromY = p_y }
                }
            }

            if (game.canMove(m.fromX,m.fromY,m.toX,m.toY))
            {
                game.move(m.fromX,m.fromY,m.toX,m.toY,m.promotion)
                nextMoveIndex += 1
            }
            else
            {
                if (autoPlayUntil >= nextMoveIndex)
                    autoPlayUntil = -1
                return false
            }
        }
        return nextMoveIndex < moves.moves.length
    }
    /**
    Plays all moves in the history.
    */
    def playAllMoves () : Unit =
    {
        autoPlayUntil = moves.moves.length-1
        playNextMove
    }
}
