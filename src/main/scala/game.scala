
import javax.swing.SwingUtilities

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
        // Set up the board
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

    def pieceAtPosition(x:Int,y:Int) = { board(x)(y) }
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit =
    {
        val piece = pieceAtPosition(fromX,fromY)
        if (piece != null)
        {
            // Do the move
            piece.move(toX, toY)
            board(toX)(toY) = piece
            board(fromX)(fromY) = null
        }
    }
    def getKing(t:Round.Round):Piece =
    {
        for (i<-0 to 7)
        {
            for (j<-0 to 7)
            {
                val piece = pieceAtPosition(i,j)
                if (piece != null)
                    if (piece.team == t)
                        if (piece.king)
                            return piece
            }
        }
        return null
    }
}

class Game(private val canvas:Canvas, private val playerWhite:Player, private val playerBlack:Player) extends Board
{
    private var round = Round.White

    canvas.newGame(this)
    playerWhite.init(this)
    playerBlack.init(this)
    callPlayer

    private var suspended = false
    def suspend =
    {
        suspended = true
        playerWhite.stop
        playerBlack.stop
    }
    def resume =
    {
        suspended = false
        callPlayer
    }

    def getRound = {round}

    def canMove(x:Int,y:Int):Boolean =
    {
        val piece = pieceAtPosition(x,y)
        if (piece != null)
            if (piece.team == round)
                return true
        return false
    }

    def canMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Boolean =
    {
        if (!canMove(fromX,fromY))
            return false
        val p = pieceAtPosition(fromX,fromY)
        if (!p.canMove(toX, toY))
            return false

        // Check if the king become check
        val cboard = new Board(this)
        cboard.move(fromX,fromY,toX,toY)
        if (cboard.getKing(round).isCheck)
            return false

        return true
    }

    private def hasPossibleMove : Boolean =
    {
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
                                return true
                        }
                    }
                }
            }
        }
        return false
    }
    private def callPlayer : Unit =
    {
        if (suspended || round == Round.Finished)
            return
        SwingUtilities.invokeLater(new Runnable() {
            override def run : Unit =
            { 
                if (suspended || round == Round.Finished)
                    return
                if (round == Round.White)
                    playerWhite.mustPlay
                if (round == Round.Black)
                    playerBlack.mustPlay
            }
        });
    }
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int):Unit =
    {
        if (suspended || round == Round.Finished)
            return

        if (!SwingUtilities.isEventDispatchThread())
        {
            SwingUtilities.invokeLater(new Runnable() {
                override def run  : Unit = { move(fromX,fromY,toX,toY) }
            });
            return
        }

        if (!canMove(fromX,fromY,toX,toY))
            return

        super.move (fromX,fromY,toX,toY)
        round = Round.adv(round)

        // Check/Checkmate/Stalemate detection
        val check = getKing(round).isCheck
        val endOfGame = !hasPossibleMove

        if (check && endOfGame)
            canvas.setMessage ("Checkmate ! " + Round.adv(round).toString + " wins !")
        else if (check)
            canvas.setMessage ("Check !")
        else if (endOfGame)
            canvas.setMessage ("Stalemate !")
        else
            canvas.clearMessage

        if (endOfGame)
            round = Round.Finished
        
        canvas.repaint
        callPlayer
    }

}