
import javax.swing.SwingUtilities

/**
Implements a chessboard with all rules (logic of moves, rounds, end of the game, etc).

All methods are thread-safe.

@param canvas The canvas that will display the game.
@param playerWhite The player of the white team.
@param playerBlack The player of the black team.
*/
class Game(private val canvas:Canvas, private val playerWhite:Player, private val playerBlack:Player) extends Board
{
    private var round = Round.White
    private var suspended = false
    private var fmRule = 0
    private var roundNumber = 0
    private var enPassantPosition : Option[(Int,Int)] = None

    canvas.newGame(this)
    playerWhite.init(this)
    playerBlack.init(this)
    playerWhite.mustPlay

    /**
    Suspends the game and every running thread. Game can be resumed later.
    */
    def suspend =
    {
        round.synchronized
        {
            if (!suspended && round != Round.Finished)
            {
                suspended = true
                playerWhite.stop
                playerBlack.stop
            }
        }
    }
    /**
    Resumes a suspended game.
    */
    def resume =
    {
        round.synchronized
        {
            if (suspended && round != Round.Finished)
            {
                suspended = false
                playerWhite.init(this)
                playerBlack.init(this)
                if (round == Round.White)
                    playerWhite.mustPlay
                if (round == Round.Black)
                    playerBlack.mustPlay
            }
        }
    }

    /**
    Returns the current round.
    */
    def getRound = { round.synchronized{ round } }

    /**
    Returns the current round number.
    */
    def getRoundNumber = { round.synchronized{ roundNumber/2 + 1 } }

    /**
    Returns the current round number in the context of the 50-move rule.
    */
    def getFiftyMoveRuleNumber = { round.synchronized{ fmRule/2 } }

    /**
    Indicates whether the piece at the given position can be moved.
    */
    def canMove(x:Int,y:Int):Boolean =
    {
        round.synchronized
        {
            val piece = pieceAtPosition(x,y)
            if (piece != null)
                if (piece.team == round)
                    return true
            return false
        }
    }

    /**
    Return None if the move is not a valid 'en passant' move. Otherwise, return the position of the eaten piece.
    */
    def enPassantMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Option[(Int,Int)] =
    {
        round.synchronized
        {
            enPassantPosition match
            {
                case None => {return None}
                case Some ((fX,fY)) => { if (fX != toX || fY != toY) return None }
            }
            if (!canMove(fromX,fromY))
                return None
            val p = pieceAtPosition(fromX,fromY)
            if (p.pieceType != PieceType.Pawn)
                return None
            if (math.abs(fromX-toX) != 1)
                return None
            if (round == Round.White && fromY-toY != 1)
                return None
            if (round == Round.Black && toY-fromY != 1)
                return None

            // Not check
            val cboard = new Board(this)
            cboard.move(fromX,fromY,toX,toY)
            cboard.remove(toX,fromY)
            if (cboard.getKing(round).isCheck)
                return None
            return Some (toX,fromY)
        }
    }
    /**
    Return None if the move is not a valid castling move. Otherwise, return the move implied for the rook.
    */
    def castlingMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Option[(Int,Int,Int,Int)] =
    {
        round.synchronized
        {
            // 1. King and rook have not moved previously in this game
            if (!canMove(fromX,fromY))
                return None
            val p = pieceAtPosition(fromX,fromY)
            if (p.pieceType != PieceType.King || p.hasMoved)
                return None
            if (fromY != toY || math.abs(toX-fromX) != 2)
                return None
            val dir = Direction.directionApplied(fromX,fromY,toX,toY)

            var rookX = fromX
            var rookY = fromY
            if (dir == Direction.Left)
                rookX = 0
            if (dir == Direction.Right)
                rookX = 7
            if (!canMove(rookX,rookY))
                return None
            val rook = pieceAtPosition(rookX,rookY)
            if (rook.pieceType != PieceType.Rook || rook.hasMoved)
                return None

            // 2. No piece between king and rook
            if (!p.noObstacleTo(rookX, rookY))
                return None

            // 3. Not check at the first/intermediate/last position
            val cboard = new Board(this)
            if (cboard.getKing(round).isCheck)
                return None
            val (intermediateX, intermediateY) = Direction.applyDirection(fromX, fromY, dir)
            cboard.move(fromX,fromY,intermediateX,intermediateY)
            if (cboard.getKing(round).isCheck)
                return None
            cboard.move(intermediateX,intermediateY,toX,toY)
            if (cboard.getKing(round).isCheck)
                return None

            return Some(rookX,rookY,intermediateX,intermediateY)
        }
    }

    /**
    Indicates whether the piece at the first position can be moved to the second position.
    */
    def canMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Boolean =
    {
        round.synchronized
        {
            if (!canMove(fromX,fromY))
                return false

            // Special move : Castling, 'En Passant'
            if (castlingMove(fromX,fromY,toX,toY) != None || enPassantMove(fromX,fromY,toX,toY) != None)
                return true
                
            // Regular move
            val p = pieceAtPosition(fromX,fromY)
            if (!p.canMove(toX, toY))
                return false

            // Checking if the king become check
            val cboard = new Board(this)
            cboard.move(fromX,fromY,toX,toY)
            if (cboard.getKing(round).isCheck)
                return false
            return true
        }
    }
    /**
    Returns a list of all the possible moves.
    */
    def possibleMoves : scala.collection.mutable.MutableList[(Int,Int,Int,Int)] =
    {
        round.synchronized
        {
            val lst = new scala.collection.mutable.MutableList[(Int,Int,Int,Int)]
            for (i<-0 to 7)
            {
                for (j<-0 to 7)
                {
                    if (canMove(i,j))
                    {
                        for (k<-0 to 7)
                        {
                            for (l<-0 to 7)
                            {
                                if (canMove(i,j,k,l))
                                    lst += ((i,j,k,l))
                            }
                        }
                    }
                }
            }
            return lst
        }
    }
    /**
    Indicates whether a draw can be requested by the current player.
    */
    def canRequestDraw():Boolean =
    {
        round.synchronized
        {
            if (round == Round.Finished)
                return false
            if (fmRule >= 100)
                return true
            return false
        }
    }
    /**
    Request draw. The request must be legit (50-move rule...)
    */
    def requestDraw():Unit =
    {
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _requestDraw }
        });
    }
    private def _requestDraw():Unit =
    {
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return
            
                // TODO
        }
    }
    /**
    Abandon the game. The opponent will win the game.
    */
    def abandon():Unit =
    {
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _abandon }
        });
    }
    private def _abandon():Unit =
    {
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return
            
                // TODO
        }
    }
    /**
    Plays the given move. The move must be legal.

    The last optional parameter is the type of piece wanted in the case of a promotion (default is queen).
    */
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int, promotionType:PieceType.PieceType):Unit =
    {
        // We must be on the main thread before making some modifications.
        // if (!SwingUtilities.isEventDispatchThread())
        // Even if we are already on the main thread, we must reinvoke this function later (avoid interlaced turns).
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _move(fromX,fromY,toX,toY,promotionType) }
        });
    }
    private def _move(fromX:Int,fromY:Int,toX:Int,toY:Int,promotionType:PieceType.PieceType):Unit =
    {
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return
            
            if (!canMove(fromX,fromY,toX,toY))
                return
            
            // Fifty-move rule updating
            fmRule += 1
            if (pieceAtPosition(fromX,fromY).pieceType == PieceType.Pawn) // Detect if the piece moved is a pawn
                fmRule = 0
            else if (pieceAtPosition(toX,toY) != null) // Detect if a piece is eaten
                fmRule = 0
            else if (enPassantMove(fromX,fromY,toX,toY) != None) // Detect if a piece is eaten 'en passant'
                fmRule = 0
            roundNumber += 1

            // Do the move !!!
            castlingMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY,tX,tY)) => {super.move (fX,fY,tX,tY)}
            }
            enPassantMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY)) => {super.remove (fX,fY)}
            }
            enPassantPosition = None
            if (pieceAtPosition(fromX,fromY).pieceType == PieceType.Pawn && math.abs(toY-fromY) == 2)
                enPassantPosition = Some (fromX+(toX-fromX)/2, fromY+(toY-fromY)/2)
            super.move (fromX,fromY,toX,toY)
            if ((toY == 0 || toY == 7) && pieceAtPosition(toX,toY).pieceType == PieceType.Pawn)
            {
                if (promotionType == PieceType.Knight)
                    super.add(new Knight(round, this, toX, toY))
                else if (promotionType == PieceType.Bishop)
                    super.add(new Bishop(round, this, toX, toY))
                else if (promotionType == PieceType.Rook)
                    super.add(new Rook(round, this, toX, toY))
                else
                    super.add(new Queen(round, this, toX, toY))
            }
            round = Round.adv(round)
        
            // Check/Checkmate/Stalemate detection
            val check = getKing(round).isCheck
            val endOfGame = possibleMoves.isEmpty

            if (check && endOfGame)
                canvas.setMessage ("Checkmate ! " + Round.adv(round).toString + " wins !")
            else if (check)
                canvas.setMessage ("Check !")
            else if (endOfGame)
                canvas.setMessage ("Stalemate !")
            else
                canvas.clearMessage

            if (endOfGame)
            {
                round = Round.Finished
                playerWhite.stop
                playerBlack.stop
            }

            canvas.repaint
            if (round == Round.White)
                playerWhite.mustPlay
            if (round == Round.Black)
                playerBlack.mustPlay
        }
    }

    override def remove(fromX:Int,fromY:Int) : Unit = { }
    override def add(p:Piece):Unit = { }
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit = { move(fromX,fromY,toX,toY,PieceType.Unknown) }

}
