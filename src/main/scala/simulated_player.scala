/**
A player that uses a preprogrammed list of moves.

You should give the canvas as a parameter in order to improve the graphic rendering.
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
            if (game.getMoveNumber >= autoPlayUntil || game.getMoveNumber+1 >= history.moves.length)
                disableAutoPlay
        
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
                val moves = game.possibleMoves
                for (move <- moves)
                {
                    val (f_x,f_y,t_x,t_y) = move
                    if (t_x == m.toX && t_y == m.toY)
                        if (game.pieceAtPosition(f_x,f_y).pieceType == m.pieceType || m.pieceType == PieceType.Unknown)
                            if ( (f_x == m.fromX || m.fromX < 0) && (f_y == m.fromY || m.fromY < 0) )
                            { m.fromX = f_x ; m.fromY = f_y }
                }
            }

            if (game.canMove(m.fromX,m.fromY,m.toX,m.toY))
            {
                game.move(m.fromX,m.fromY,m.toX,m.toY,m.promotion)
                canPlay = false
            }
            else
            {
                disableAutoPlay
                if (canvas != null)
                    canvas.repaint
                return false
            }
            return game.getMoveNumber < history.moves.length
        }
        return false
    }
    private def disableAutoPlay() : Unit =
    {
        if (autoPlayUntil >= 0)
        {
            autoPlayUntil = -1
            if (canvas != null)
                canvas.ignoreRepaint = false
        }
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
    */
    def playUntilIndex (i:Int) : Unit =
    {
        if (game != null)
        {
            if (i >= game.getMoveNumber)
            {
                if (canvas != null)
                    canvas.ignoreRepaint = true
                autoPlayUntil = i
                playNextMove
            }
            else
                disableAutoPlay
        }
    }
}
