
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
                val p = _b.board(i)(j)
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
            val piece = board(fromX)(fromY)
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
    Add a piece to the board.
    */
    def add(p:Piece):Unit =
    {
        board.synchronized { addPiece(p) }
    }
    /**
    Remove the piece at a given position.
    */
    def remove(fromX:Int,fromY:Int) : Unit =
    {
        board.synchronized { board(fromX)(fromY) = null }
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
                    val piece = board(i)(j)
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
