
class Pawn(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/Chess_plt60.png"
    if(team == Round.Black)
        image_path = "img/Chess_pdt60.png"

    private var firstMove = true

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;

        if (team == Round.White) // Pawn must go up
        {
            // Not an eating move
            if (board.pieceAtPosition(toX, toY) == null)
            {
                if (x != toX)
                    return false;
                if (y-toY < 1 || y-toY > 2)
                    return false;
                if (y-toY == 2 && (!firstMove || board.pieceAtPosition(x,y-1) != null))
                    return false;
            }
            else // Eating move
            {
                if (math.abs (x - toX) != 1)
                    return false;
                if (y - toY != 1)
                    return false;
            }
        }
        if (team == Round.Black) // Pawn must go down
        {
            // Not an eating move
            if (board.pieceAtPosition(toX, toY) == null)
            {
                if (toX != x)
                    return false;
                if (toY-y < 1 || toY-y > 2)
                    return false;
                if (toY-y == 2 && (!firstMove || board.pieceAtPosition(x,y+1) != null))
                    return false;
            }
            else // Eating move
            {
                if (math.abs (toX - x) != 1)
                    return false;
                if (toY - y != 1)
                    return false;
            }
        }
        return true;
    }
    override def move(toX:Int,toY:Int): Unit =
    {
        super.move(toX,toY);
        firstMove = false;
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
        image_path = "img/Chess_rlt60.png"
    if(team == Round.Black)
        image_path = "img/Chess_rdt60.png"

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isStraight(dir))
        {
            if (noObstacleTo(toX,toY))
                return true
        }
        return false
    }
    override def copy(b:Board) : Piece = { new Rook(team, b, x, y) }
}

class Bishop(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/Chess_blt60.png"
    if(team == Round.Black)
        image_path = "img/Chess_bdt60.png"

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isDiag(dir))
        {
            if (noObstacleTo(toX,toY))
                return true
        }
        return false
    }
    override def copy(b:Board) : Piece = { new Bishop(team, b, x, y) }
}

class Knight(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/Chess_nlt60.png"
    if(team == Round.Black)
        image_path = "img/Chess_ndt60.png"

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        if (math.abs(x-toX) == 2)
            if (math.abs(y-toY) == 1)
                return true

        if (math.abs(y-toY) == 2)
            if (math.abs(x-toX) == 1)
                return true

        return false
    }
    override def copy(b:Board) : Piece = { new Knight(team, b, x, y) }
}

class King(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/Chess_klt60.png"
    if(team == Round.Black)
        image_path = "img/Chess_kdt60.png"
        
    override val king = true

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        if (math.abs(x-toX) <= 1 && math.abs(y-toY) <= 1)
            return true

        return false
    }
    override def copy(b:Board) : Piece = { new King(team, b, x, y) }
}

class Queen(private val _team:Round.Round, private val _board:Board, private val _x:Int, private val _y:Int) extends Piece(_team, _board, _x, _y)
{
    if(team == Round.White)
        image_path = "img/Chess_qlt60.png"
    if(team == Round.Black)
        image_path = "img/Chess_qdt60.png"

    override def canMove(toX:Int,toY:Int): Boolean =
    {
        if (!super.canMove(toX,toY))
            return false;
        
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (Direction.isStraight(dir) || Direction.isDiag(dir))
        {
            if (noObstacleTo(toX,toY))
                return true
        }
        return false
    }
    override def copy(b:Board) : Piece = { new Queen(team, b, x, y) }
}

