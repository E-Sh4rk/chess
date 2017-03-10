
import javax.swing.SwingUtilities

/**
Enumeration that represents a round : White, Black, Finished.
*/
object Round extends Enumeration {
    type Round = Value
    val White, Black, Finished = Value
    def adv(t:Round.Round):Round.Round =
    {
        if (t == Round.White)
            return Round.Black
        if (t == Round.Black)
            return Round.White
        return Round.Finished
    }
}

/**
Implements a simple chessboard without any rule.

All methods are thread-safe.

@param _b The board to copy. If null or not present : initializes a new chessboard.
*/
class Board (private val _b:Board)
{
    private val board = Array.ofDim[Piece](8,8)
    private def addPiece(p:Piece):Unit =
    {
        val (x,y) = p.getPosition
        board(x)(y) = p
    }
    if (_b == null)
    {
        // Setting up the board
        for (x<-0 to 7)
        {
            addPiece(new Pawn(Round.Black, this, x, 1))
            addPiece(new Pawn(Round.White, this, x, 6))
        }
        addPiece(new Rook(Round.Black, this, 0, 0))
        addPiece(new Rook(Round.Black, this, 7, 0))
        addPiece(new Knight(Round.Black, this, 1, 0))
        addPiece(new Knight(Round.Black, this, 6, 0))
        addPiece(new Bishop(Round.Black, this, 2, 0))
        addPiece(new Bishop(Round.Black, this, 5, 0))
        addPiece(new Queen(Round.Black, this, 3, 0))
        addPiece(new King(Round.Black, this, 4, 0))
        addPiece(new Rook(Round.White, this, 0, 7))
        addPiece(new Rook(Round.White, this, 7, 7))
        addPiece(new Knight(Round.White, this, 1, 7))
        addPiece(new Knight(Round.White, this, 6, 7))
        addPiece(new Bishop(Round.White, this, 2, 7))
        addPiece(new Bishop(Round.White, this, 5, 7))
        addPiece(new Queen(Round.White, this, 3, 7))
        addPiece(new King(Round.White, this, 4, 7))
    }
    else
    {
        // Constructor to make a copy
        for (i<-0 to 7)
        {
            for (j<-0 to 7)
            {
                val p = _b.pieceAtPosition(i,j)
                if (p != null)
                    board(i)(j) = p.copy(this)
            }
        }
    }

    def this () = { this (null) }

    /**
    Returns the piece at the given position. Return null if there is no piece at this position.
    */
    def pieceAtPosition(x:Int,y:Int) = { board.synchronized { board(x)(y) } }
    /**
    Moves a piece from a position to another position.
    */
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit =
    {
        board.synchronized
        {
            val piece = pieceAtPosition(fromX,fromY)
            if (piece != null)
            {
                // Doing the move
                piece.move(toX, toY)
                board(toX)(toY) = piece
                board(fromX)(fromY) = null
            }
        }
    }
    /**
    Returns the king of the given team. Return null if not found.
    */
    def getKing(t:Round.Round):Piece =
    {
        board.synchronized
        {
            for (i<-0 to 7)
            {
                for (j<-0 to 7)
                {
                    val piece = pieceAtPosition(i,j)
                    if (piece != null)
                        if (piece.team == t)
                            if (piece.pieceType == PieceType.King)
                                return piece
                }
            }
            return null
        }
    }
}

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
    private val fmRule = scala.collection.mutable.Map[Round.Round, Int]()

    fmRule(Round.White) = 0
    fmRule(Round.Black) = 0
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
    Determine whether a move is a valid 'En Passant' move or not.
    */
    def isEnPassantMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Boolean =
    {
        round.synchronized
        {
            // TODO
            return false
        }
    }
    /**
    Return None if the move is not a castling move. Otherwise, return the move implied for the rook.
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

            // 3. No check at the first/intermediate/last case
            if (getKing(round).isCheck)
                return None
            val (intermediateX, intermediateY) = Direction.applyDirection(fromX, fromY, dir)
            var cboard = new Board(this)
            cboard.move(fromX,fromY,intermediateX,intermediateY)
            if (cboard.getKing(round).isCheck)
                return None
            cboard = new Board(this)
            cboard.move(fromX,fromY,toX,toY)
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
            val p = pieceAtPosition(fromX,fromY)
            if (!p.canMove(toX, toY))
            {
                // Exception for special moves : Castling, 'En Passant'
                if (castlingMove(fromX,fromY,toX,toY) == None && !isEnPassantMove(fromX,fromY,toX,toY))
                    return false
            }

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
    Plays the given move. The move must be legal.
    */
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int):Unit =
    {
        // We must be on the main thread before making some modifications.
        // if (!SwingUtilities.isEventDispatchThread())
        // Even if we are already on the main thread, we must reinvoke this function later (avoid interlaced turns).
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _move(fromX,fromY,toX,toY) }
        });
    }
    private def _move(fromX:Int,fromY:Int,toX:Int,toY:Int):Unit =
    {
        // TODO : Cas du castling et en passant : Faire la deuxième action
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return
            
            if (!canMove(fromX,fromY,toX,toY))
                return
            
            // fifty-move rule updating
            // TODO : detect if the piece moved is a pawn
            if (pieceAtPosition(toX,toY) != null)
            {
                fmRule(Round.White) = 0
                fmRule(Round.Black) = 0
            }
            else
            {
                fmRule(round) += 1
            }

            // Do the move !!!
            castlingMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY,tX,tY)) => {super.move (fX,fY,tX,tY)}
            }
            super.move (fromX,fromY,toX,toY)
            round = Round.adv(round)
        
            // Check/Checkmate/Stalemate detection
            val check = getKing(round).isCheck
            val endOfGame = possibleMoves.isEmpty
            val fmRulePossible = fmRule(round) >= 50

            if (check && endOfGame)
                canvas.setMessage ("Checkmate ! " + Round.adv(round).toString + " wins !")
            else if (check)
                canvas.setMessage ("Check !")
            else if (endOfGame)
                canvas.setMessage ("Stalemate !")
            else if (fmRulePossible)
                canvas.setMessage ("Fifty-move rule !")
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

}
