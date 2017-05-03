
/**
Enumeration that represents the game mode (variant)
*/
object GameMode extends Enumeration {
    type GameMode = Value
    val Vanilla, Janus, Capablanca = Value
}

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

@param _gm The game mode (it will determine default position of pieces).
@param _b The board to copy. If null or not present : initializes a new chessboard.
@param dim_x The x-dimension of the chessboard.
@param dim_y The y-dimension of the chessboard.
*/
class Board (private val _b:Board, private val _gm:GameMode.GameMode)
{
    val dim_x:Int = if (_b == null) (if (_gm == GameMode.Vanilla) 8 else 10) else _b.dim_x
    val dim_y:Int = if (_b == null) 8 else _b.dim_y
    private val board = Array.ofDim[Piece](dim_x,dim_y)

    private def addPiece(p:Piece):Unit =
    {
        val (x,y) = p.getPosition
        board(x)(y) = p
    }

    if (_b == null)
    {
        // Setting up the board
        for (x<-0 to dim_x-1)
        {
            addPiece(new Pawn(Round.Black, this, x, 1))
            addPiece(new Pawn(Round.White, this, x, dim_y-2))
        }
        addPiece(new Rook(Round.Black, this, 0, 0))
        addPiece(new Rook(Round.Black, this, dim_x-1, 0))
        addPiece(new Rook(Round.White, this, 0, dim_y-1))
        addPiece(new Rook(Round.White, this, dim_x-1, dim_y-1))

        if (_gm == GameMode.Vanilla)
        {
            addPiece(new Knight(Round.Black, this, 1, 0))
            addPiece(new Knight(Round.Black, this, dim_x-2, 0))
            addPiece(new Bishop(Round.Black, this, 2, 0))
            addPiece(new Bishop(Round.Black, this, dim_x-3, 0))
            addPiece(new Queen(Round.Black, this, 3, 0))
            addPiece(new King(Round.Black, this, dim_x-4, 0))

            addPiece(new Knight(Round.White, this, 1, dim_y-1))
            addPiece(new Knight(Round.White, this, dim_x-2, dim_y-1))
            addPiece(new Bishop(Round.White, this, 2, dim_y-1))
            addPiece(new Bishop(Round.White, this, dim_x-3, dim_y-1))
            addPiece(new Queen(Round.White, this, 3, dim_y-1))
            addPiece(new King(Round.White, this, dim_x-4, dim_y-1))
        }
        if (_gm == GameMode.Janus)
        {
            addPiece(new ArchBishop(Round.Black, this, 1, 0))
            addPiece(new ArchBishop(Round.Black, this, dim_x-2, 0))
            addPiece(new Knight(Round.Black, this, 2, 0))
            addPiece(new Knight(Round.Black, this, dim_x-3, 0))
            addPiece(new Bishop(Round.Black, this, 3, 0))
            addPiece(new Bishop(Round.Black, this, dim_x-4, 0))
            addPiece(new King(Round.Black, this, 4, 0))
            addPiece(new Queen(Round.Black, this, dim_x-5, 0))

            addPiece(new ArchBishop(Round.White, this, 1, dim_y-1))
            addPiece(new ArchBishop(Round.White, this, dim_x-2, dim_y-1))
            addPiece(new Knight(Round.White, this, 2, dim_y-1))
            addPiece(new Knight(Round.White, this, dim_x-3, dim_y-1))
            addPiece(new Bishop(Round.White, this, 3, dim_y-1))
            addPiece(new Bishop(Round.White, this, dim_x-4, dim_y-1))
            addPiece(new King(Round.White, this, 4, dim_y-1))
            addPiece(new Queen(Round.White, this, dim_x-5, dim_y-1))
        }
        if (_gm == GameMode.Capablanca)
        {
            addPiece(new Knight(Round.Black, this, 1, 0))
            addPiece(new Knight(Round.Black, this, dim_x-2, 0))
            addPiece(new ArchBishop(Round.Black, this, 2, 0))
            addPiece(new Chancellor(Round.Black, this, dim_x-3, 0))
            addPiece(new Bishop(Round.Black, this, 3, 0))
            addPiece(new Bishop(Round.Black, this, dim_x-4, 0))
            addPiece(new Queen(Round.Black, this, 4, 0))
            addPiece(new King(Round.Black, this, dim_x-5, 0))
            
            addPiece(new Knight(Round.White, this, 1, dim_y-1))
            addPiece(new Knight(Round.White, this, dim_x-2, dim_y-1))
            addPiece(new ArchBishop(Round.White, this, 2, dim_y-1))
            addPiece(new Chancellor(Round.White, this, dim_x-3, dim_y-1))
            addPiece(new Bishop(Round.White, this, 3, dim_y-1))
            addPiece(new Bishop(Round.White, this, dim_x-4, dim_y-1))
            addPiece(new Queen(Round.White, this, 4, dim_y-1))
            addPiece(new King(Round.White, this, dim_x-5, dim_y-1))
        }
    }
    else
    {
        // Constructor to make a copy
        for (i<-0 to dim_x-1)
        {
            for (j<-0 to dim_y-1)
            {
                val p = _b.board(i)(j)
                if (p != null)
                    board(i)(j) = p.copy(this)
            }
        }
    }
    def this (_b:Board) = { this (_b, GameMode.Vanilla) }
    def this (_gm:GameMode.GameMode) = { this (null, _gm) }
    def this () = { this (GameMode.Vanilla) }

    /**
    Returns the piece at the given position. Return null if there is no piece at this position.
    */
    def pieceAtPosition(x:Int,y:Int) : Piece =
    {
        this.synchronized
        {
            if (x < 0 || y < 0 || x >= dim_x || y >= dim_y)
                return null
            return board(x)(y)
        }
    }
    /**
    Moves a piece from a position to another position.
    */
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit =
    {
        this.synchronized
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
        this.synchronized { addPiece(p) }
    }
    /**
    Remove the piece at a given position.
    */
    def remove(fromX:Int,fromY:Int) : Unit =
    {
        this.synchronized { board(fromX)(fromY) = null }
    }
    /**
    Returns the king of the given team. Return null if not found.
    */
    def getKing(t:Round.Round):Piece =
    {
        this.synchronized
        {
            val pieces = getPieces(t,PieceType.King)
            if(pieces.length > 0)
                return pieces(0)
            return null
        }
    }
    /**
    Returns the pieces that correspond to the given type and team.
    */
    def getPieces(t:Round.Round,ptype:PieceType.PieceType):scala.collection.mutable.MutableList[Piece] =
    {
        this.synchronized
        {
            val res = new scala.collection.mutable.MutableList[Piece]()
            for (i<-0 to dim_x-1)
            {
                for (j<-0 to dim_y-1)
                {
                    val piece = board(i)(j)
                    if (piece != null)
                        if (piece.team == t)
                            if (piece.pieceType == ptype)
                                res += piece
                }
            }
            return res
        }
    }
}
