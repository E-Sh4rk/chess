
class Pawn(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_pawn.png"
    if(team == Round.Black)
        image_path = "img/b_pawn.png"

    override val pieceType = PieceType.Pawn

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;

        if (team == Round.White && toY >= y)
            return false;
        if (team == Round.Black && toY <= y)
            return false;

        // Not an eating move
        if (board.pieceAtPosition(toX, toY) == null)
        {
            if (x != toX)
                return false;
            if (math.abs(y-toY) > 2 || toY == y)
                return false;
            if (math.abs(y-toY) == 2 && (!firstMove || board.pieceAtPosition(x,(y+toY)/2) != null))
                return false;
        }
        else // Eating move
        {
            if (math.abs(x-toX) != 1)
                return false;
            if (math.abs(y-toY) != 1)
                return false;
        }
        return true;
    }
    override def copy(b:Board) : Piece =
    {
        val p = new Pawn(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class Rook(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_rook.png"
    if(team == Round.Black)
        image_path = "img/b_rook.png"

    override val pieceType = PieceType.Rook

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isStraight(dir))
            if (noObstacleTo(toX,toY))
                return true

        return false
    }
    override def copy(b:Board) : Piece =
    {
        val p = new Rook(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class Bishop(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_bishop.png"
    if(team == Round.Black)
        image_path = "img/b_bishop.png"

    override val pieceType = PieceType.Bishop

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isDiag(dir))
            if (noObstacleTo(toX,toY))
                return true

        return false
    }
    override def copy(b:Board) : Piece =
    {
        val p = new Bishop(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class Knight(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_knight.png"
    if(team == Round.Black)
        image_path = "img/b_knight.png"

    override val pieceType = PieceType.Knight

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        return Direction.isKnightMove(x,y,toX,toY)
    }
    override def copy(b:Board) : Piece =
    {
        val p = new Knight(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class King(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_king.png"
    if(team == Round.Black)
        image_path = "img/b_king.png"
        
    override val pieceType = PieceType.King

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        if (math.abs(x-toX) <= 1 && math.abs(y-toY) <= 1)
            return true

        return false
    }
    override def copy(b:Board) : Piece =
    {
        val p = new King(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class Queen(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_queen.png"
    if(team == Round.Black)
        image_path = "img/b_queen.png"

    override val pieceType = PieceType.Queen

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isStraight(dir) || Direction.isDiag(dir))
            if (noObstacleTo(toX,toY))
                return true

        return false
    }
    override def copy(b:Board) : Piece =
    {
        val p = new Queen(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class ArchBishop(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_archbishop.png"
    if(team == Round.Black)
        image_path = "img/b_archbishop.png"

    override val pieceType = PieceType.ArchBishop

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;

        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isDiag(dir))
            if (noObstacleTo(toX,toY))
                return true
        
        return Direction.isKnightMove(x,y,toX,toY)
    }
    override def copy(b:Board) : Piece =
    {
        val p = new ArchBishop(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}

class Chancellor(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/w_chancellor.png"
    if(team == Round.Black)
        image_path = "img/b_chancellor.png"

    override val pieceType = PieceType.Chancellor

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;

        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isStraight(dir))
            if (noObstacleTo(toX,toY))
                return true
        
        return Direction.isKnightMove(x,y,toX,toY)
    }
    override def copy(b:Board) : Piece =
    {
        val p = new Chancellor(team, b, x, y)
        p.firstMove = this.firstMove
        return p
    }
}
