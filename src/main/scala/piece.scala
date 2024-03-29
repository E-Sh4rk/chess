
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/**
Enumeration that represents a direction (diagonals included).
*/
object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right, UpRight, UpLeft, DownRight, DownLeft, NoDirection = Value

    /**
    Indicates whether a direction is straight (not diagonal).
    */
    def isStraight (dir:Direction.Direction) : Boolean =
    {
        return dir == Direction.Up || dir == Direction.Down || dir == Direction.Left || dir == Direction.Right
    }
    /**
    Indicates whether a direction is diagonal.
    */
    def isDiag (dir:Direction.Direction) : Boolean =
    {
        return dir == Direction.UpLeft || dir == Direction.DownLeft || dir == Direction.UpRight || dir == Direction.DownRight
    }
    /**
    Indicates whether a move is a knight move.
    */
    def isKnightMove (fromX:Int,fromY:Int,toX:Int,toY:Int) : Boolean =
    {
        if (math.abs(fromX-toX) == 2)
            if (math.abs(fromY-toY) == 1)
                return true

        if (math.abs(fromY-toY) == 2)
            if (math.abs(fromX-toX) == 1)
                return true

        return false
    }
    /**
    Returns the direction that has been applied to go from the first position to the second.
    */
    def directionApplied (fromX:Int, fromY:Int, toX:Int, toY:Int) : Direction.Direction =
    {
        if (fromX == toX)
        {
            if (fromY < toY)
                return Direction.Down
            if (fromY > toY)
                return Direction.Up
        }
        if (fromY == toY)
        {
            if (fromX < toX)
                return Direction.Right
            if (fromX > toX)
                return Direction.Left
        }
        if (toX - fromX == toY - fromY)
        {
            if (fromX < toX)
                return Direction.DownRight
            if (fromX > toX)
                return Direction.UpLeft
        }
        if (toX - fromX == fromY - toY)
        {
            if (fromX < toX)
                return Direction.UpRight
            if (fromX > toX)
                return Direction.DownLeft
        }
        return Direction.NoDirection
    }
    /**
    Returns the position obtained by moving a case in the given direction from the given position.
    */
    def applyDirection (fromX:Int, fromY:Int, dir:Direction.Direction):(Int,Int) =
    {
        if (dir == Direction.Up)
            return (fromX,fromY-1)
        if (dir == Direction.Down)
            return (fromX,fromY+1)
        if (dir == Direction.Left)
            return (fromX-1,fromY)
        if (dir == Direction.Right)
            return (fromX+1,fromY)
        if (dir == Direction.UpLeft)
            return (fromX-1,fromY-1)
        if (dir == Direction.DownLeft)
            return (fromX-1,fromY+1)
        if (dir == Direction.UpRight)
            return (fromX+1,fromY-1)
        if (dir == Direction.DownRight)
            return (fromX+1,fromY+1)
        return (fromX,fromY)
    }
}

object PieceType extends Enumeration {
    type PieceType = Value
    val Pawn, Rook, Bishop, Knight, King, Queen, ArchBishop, Chancellor, Unknown = Value
}

/**
Companion object for piece in order to have a cache for the loading of the images.
*/
object Piece
{
    private val images = scala.collection.mutable.HashMap.empty[String,BufferedImage]
    private def loadImage (path:String) =
    {
        if (images contains path)
            images(path)
        else
        {
            val img = ImageIO.read(new File(path))
            images += ( path -> img )
            img
        }
    }
}
/**
Represents a piece on the chessboard.
*/
abstract class Piece(val team:Round.Round, protected val board:Board, protected var x:Int, protected var y:Int)
{
    protected var image_path = ""
    protected var firstMove = true
    
    /**
    Indicates the type of the piece.
    */
    val pieceType = PieceType.Unknown

    /**
    Indicates whether there is an obstacle between the piece and the position given (excluded).
    */
    def noObstacleTo (toX:Int,toY:Int):Boolean =
    {
        val dir = Direction.directionApplied(x,y,toX,toY)
        if (dir == Direction.NoDirection)
            return true

        var (ix,iy) = Direction.applyDirection(x,y,dir)
        while (ix != toX || iy != toY)
        {
            if (board.pieceAtPosition(ix,iy) != null)
                return false
            Direction.applyDirection(ix,iy,dir) match { case (nx,ny) => { ix = nx; iy = ny } }
        }
        return true
    }

    /**
    Returns the current position of the piece on the chessboard.
    */
    def getPosition = { (x,y) }

    /**
    Returns the buffered image that represents the piece.
    */
    def getImage = { Piece.loadImage(image_path) }

    /**
    Returns whether the other player is checked.
    */
    def isCheck : Boolean =
    {
        for (i<-0 to board.dim_x-1)
        {
            for (j<-0 to board.dim_y-1)
            {
                val piece = board.pieceAtPosition(i,j)
                if (piece != null)
                    if (piece.team == Round.adv(team))
                        if (piece.canMove(x,y))
                            return true
            }
        }
        return false
    }

    /**
    Returns whether the piece can move to the given position.

    DOES NOT TAKE INTO ACCOUNT THE ROUND, THE POTENTIAL CHECK STATUS AND THE SPECIAL RULES.
    */
    def canMove(toX:Int,toY:Int): Boolean =
    {
        if (toX < 0 || toY < 0 || toX > board.dim_x-1 || toY > board.dim_y-1)
            return false;
        val piece = board.pieceAtPosition(toX, toY);
        if (piece != null)
            if (piece.team == team)
                return false;

        return true;
    }

    /**
    Moves the piece to the given position.
    
    DOES NOT VERIFY THAT THE MOVE IS LEGAL OR NOT.
    */
    def move(toX:Int,toY:Int): Unit =
    {
        x=toX ; y=toY
        firstMove = false
    }

    /**
    Indicates whether the piece has already moved in this game.
    */
    def hasMoved : Boolean = { return !firstMove }

    /**
    Makes a copy of this piece on the new given board.
    */
    def copy(b:Board) : Piece
}

/**
An immutable structure that represents a piece at a given instant.

Not every characteristic is specified : a PieceStruct is only characterized by its position, team and type.
*/
class PieceStruct(val pieceType:PieceType.PieceType, val team:Round.Round, val pos:(Int,Int))
{
    def this (p:Piece) = { this (p.pieceType, p.team, p.getPosition) }

    override def equals(that: Any): Boolean =
        that match {
            case that: PieceStruct => this.hashCode == that.hashCode
            case _ => false
     }
    override def hashCode: Int = {
        val (x,y) = pos
        var result = (x+y)*(x+y+1)/2+y
        result = result*PieceType.maxId + pieceType.id
        result = result*Round.maxId + team.id
        return result
    }
}
