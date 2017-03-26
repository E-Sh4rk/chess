/**
A player that uses a preprogrammed list of moves.

You should give the canvas as parameter in order to improve the graphic rendering.
*/
class SimulatedPlayer(val history:History, private val canvas : Canvas = null) extends Player
{
    private var game:Game = null
    private var autoPlayUntil = -1
    private var canPlay = false

    def init (g:Game) : Unit = { game = g }
    def stop : Unit = { canPlay = false ; game = null }
    def mustPlay : Unit =
    {
        if (game == null)
            return
        canPlay = true
        if (game.getMoveNumber <= autoPlayUntil)
            playNextMove
    }

    /**
    Plays the next move.

    Returns true if, after the move, there still are new moves to play in the list, and false otherwise.
    */
    def playNextMove () : Boolean =
    {
        if (game != null && canPlay)
        {
            if (game.getMoveNumber >= history.moves.length)
            {
                disableAutoPlay
                return false
            }
        
            val m = history.moves(game.getMoveNumber)

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
                if (game.getMoveNumber >= autoPlayUntil)
                    disableAutoPlay
                game.move(m.fromX,m.fromY,m.toX,m.toY,m.promotion)
            }
            else
            {
                disableAutoPlay
                return false
            }
            if (game.getMoveNumber+1 < history.moves.length)
                return true
            disableAutoPlay
        }
        return false
    }
    private def disableAutoPlay() : Unit =
    {
        autoPlayUntil = -1
        if (canvas != null)
            canvas.ignoreRepaint = false
    }
    /**
    Plays all moves in the history.
    */
    def playAllMoves () : Unit =
    {
        playUntilIndex(history.moves.length-1)
    }
    /**
    Plays all moves until the specified index.

    Returns true if, after the move, there still are new moves to play in the list, and false otherwise.
    */
    def playUntilIndex (i:Int) : Boolean =
    {
        if (game != null)
        {
            if (i >= game.getMoveNumber)
            {
                if (canvas != null)
                    canvas.ignoreRepaint = true
                autoPlayUntil = i
                if (canPlay)
                    playNextMove
                return i + 1 < history.moves.length
            }
            else
            {
                disableAutoPlay
                return game.getMoveNumber < history.moves.length
            }
        }
        return false
    }
}
