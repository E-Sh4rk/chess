import java.awt.Graphics2D
import java.awt.Graphics
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.FontMetrics
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.swing.event._
import swing._

/**
Canvas that draws the chessboard and catches user clicks.

It is the only interface between the current game and the user.
It can either be used as a panel (to show the chessboard) or as a player.
*/
class Canvas(private var width:Int, private var height:Int) extends Panel with Player
{
    private var selectedCase : Option[(Int,Int)] = None
    private var selectedCase2 : Option[(Int,Int)] = None // Used for promotion
    private var game : Game = null
    private var canPlay : Boolean = false
    private var message : String = null

    /**
    Initializes the canvas for a new game.
    */
    def newGame (g:Game) : Unit =
    {
        selectedCase = None
        selectedCase2 = None
        canPlay = false
        message = null
        game = g
        repaint
    }
    /**
    Prints a message in the center of the chessboard.

    repaint must be called after.
    */
    def setMessage (s:String) : Unit = { message = s }
    /**
    Clears the message.
    
    repaint must be called after.
    */
    def clearMessage : Unit = { message = null }

    // Player methods
    def init (g:Game) : Unit = { } // The canvas use the same game for drawing as for playing
    def mustPlay : Unit = { canPlay = true }
    def stop : Unit = { canPlay = false }

    // Drawing (output)
    this.preferredSize = new Dimension(width, height)
    /**
    Resize the canvas.
    */
    def resize(w:Int, h:Int)
    {
        width = w ; height = h
        this.preferredSize = new Dimension(width, height)
        repaint
    }

    private val introImage = ImageIO.read(new File("img/Chess_intro.png"))
    private val promoQueen = ImageIO.read(new File("img/tux_queen.png"))
    private val promoKnight = ImageIO.read(new File("img/tux_knight.png"))
    private val promoBishop = ImageIO.read(new File("img/tux_bishop.png"))
    private val promoRook = ImageIO.read(new File("img/tux_rook.png"))
    override def paintComponent(g: Graphics2D) : Unit = {
        super.paintComponent(g)

        if (game == null)
        {
            // Drawing the intro image
            g.setColor(Color.black)
            if (width*2 <= height*3)
                drawCenteredString(g, "Checks", new Rectangle(0,0,width,height-width/2), g.getFont().deriveFont(g.getFont().getSize() * 2F * width / 100F))
            g.drawImage(introImage,0,height-width/2,width,width/2,null)
            return
        }

        if (selectedCase2 != None)
        {
            // Promotion !
            g.setColor(Color.white)
            g.fillRect(0,0,width,height);
            val max_height = math.min(height,width*3/2)
            val max_width = max_height*2/3
            g.drawImage(promoQueen,(width-(max_width/2))/2,0,max_width/2,max_height/2,null)
            g.drawImage(promoRook,(width/3-(max_width/3))/2,height/2,max_width/3,max_height/3,null)
            g.drawImage(promoKnight,width/3+(width/3-(max_width/3))/2,height/2,max_width/3,max_height/3,null)
            g.drawImage(promoBishop,2*width/3+(width/3-(max_width/3))/2,height/2,max_width/3,max_height/3,null)
            return
        }
        
        // Coloring the board
        for (i<-0 to 7)
        {
            for (j<-0 to 7)
            {
                if ((i+j) % 2 == 0)
                {
                    // White case
                    g.setColor(Color.white);
                    g.fillRect(i*width/8,j*height/8,width/8,height/8);
                }
                else
                {
                    // 'Black' case
                    g.setColor(new Color(220,220,220));
                    g.fillRect(i*width/8,j*height/8,width/8,height/8);
                }
                if (Some(i,j) == selectedCase)
                {
                    // Selected case : yellow
                    g.setColor(/*Color.yellow*/new Color(0xFF,0xFF,0x66));
                    g.fillRect(i*width/8,j*height/8,width/8,height/8);
                }
                // Coloring cases we can move to : green
                g.setColor(/*Color.green*/new Color(0x62,0xDD,0x62));
                selectedCase match
                {
                    case None => {}
                    case Some ((x,y)) if game.canMove(x,y,i,j) => g.fillRect(i*width/8,j*height/8,width/8,height/8);
                    case _ => {}
                }
            }
        }

        // Drawing the grid
        g.setColor(new Color(100,100,100));
        for (i<-0 to 8)
        {
            g.setStroke(new BasicStroke(2));
            g.drawLine(0,i*height/8,width,i*height/8);
            g.drawLine(i*width/8,0,i*width/8,height);
        }

        // Drawing each piece in game
        for (i<-0 to 7)
        {
            for (j<-0 to 7)
            {
                val piece = game.pieceAtPosition(i,j)
                val min = math.min(width,height)
                if (piece != null)
                    g.drawImage(piece.getImage,i*width/8+(width-min)/16,j*height/8+(height-min)/16,min/8,min/8,null)
            }
        }

        // Prints a message !
        if (message != null)
        {
            g.setColor(Color.red)
            drawCenteredString(g, message, new Rectangle(0,0,width,height), g.getFont().deriveFont(g.getFont().getSize() * 0.5F * width / 100F))
        }
    }
    private def drawCenteredString(g:Graphics, text:String, rect:Rectangle, font:Font)
    {
        val metrics:FontMetrics = g.getFontMetrics(font);
        val x:Int = (rect.width - metrics.stringWidth(text)) / 2;
        val y:Int = ((rect.height - metrics.getHeight()) / 2) + metrics.getAscent();
        g.setFont(font);
        g.drawString(text, x, y);
    }

    // Events (input)
    listenTo(mouse.clicks)
    reactions +=
    {
        case MouseClicked(src, pt, mod, clicks, pops) => {
            if (pt.x >= 0 && pt.y >= 0 && pt.x <= width && pt.y <= height)
            {
                val x = pt.x * 8 / width
                val y = pt.y * 8 / height

                if (game != null && canPlay)
                {
                    if (selectedCase2 != None)
                    {
                        var ptype = PieceType.Unknown
                        // Promotion ! Choosing a piece type
                        if (pt.y <= height/2)
                            ptype = PieceType.Queen
                        else if (pt.x <= height/3)
                            ptype = PieceType.Rook
                        else if (pt.x <= 2*height/3)
                            ptype = PieceType.Knight
                        else if (pt.x <= height)
                            ptype = PieceType.Bishop
                        // Do the promotion move
                        val Some ((sel_x, sel_y)) = selectedCase
                        val Some ((sel_x2, sel_y2)) = selectedCase2
                        selectedCase = None
                        selectedCase2 = None
                        canPlay = false
                        game.move(sel_x, sel_y, sel_x2, sel_y2, ptype)
                    }
                    else if (selectedCase == Some(x,y))
                        selectedCase = None
                    else
                    {
                        if (game.canMove(x,y))
                            selectedCase = Some(x,y)
                        else if (selectedCase != None)
                        {
                            // Playing the move if possible
                            val Some ((sel_x, sel_y)) = selectedCase
                            if (game.canMove(sel_x, sel_y, x, y))
                            {
                                if (game.pieceAtPosition(sel_x,sel_y).pieceType == PieceType.Pawn && (y == 0 || y == 7))
                                    // Promotion move
                                    selectedCase2 = Some(x, y)
                                else
                                {
                                    // Not a promotion move
                                    selectedCase = None
                                    canPlay = false
                                    game.move(sel_x, sel_y, x, y)
                                } 
                            }
                        }
                    }
                    repaint
                }
            }
        }
    }

}
