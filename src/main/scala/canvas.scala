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
Canvas that draw the chessboard and catch user clicks.

It is the only interface bewteen the current game and the user.
It can be used either as a panel (to show the chessboard) and as a player.
*/
class Canvas(val width:Int, val height:Int) extends Panel with Player
{
    private var selectedCase : Option[(Int,Int)] = None
    private var game : Game = null
    private var canPlay : Boolean = false
    private var message : String = null

    /**
    Initialize the canvas for a new game.
    */
    def newGame (g:Game) : Unit =
    {
        selectedCase = None
        canPlay = false
        message = null
        game = g
        repaint
    }
    /**
    Print a message in the center of the chessboard.

    Repaint must be called after.
    */
    def setMessage (s:String) : Unit = { message = s }
    /**
    Clear the message.
    
    Repaint must be called after.
    */
    def clearMessage : Unit = { message = null }

    // Player methods
    def init (g:Game) : Unit = { } // The canvas use the same game for drawing as for playing
    def mustPlay : Unit = { canPlay = true }
    def stop : Unit = { canPlay = false }

    // Drawing (output)
    this.preferredSize = new Dimension(width, height)
    private val introImage = ImageIO.read(new File("img/Chess_intro.png"))
    override def paintComponent(g: Graphics2D) : Unit = {
        super.paintComponent(g)

        if (game == null)
        {
            // Draw the intro image
            g.setColor(new Color(100,100,100))
            drawCenteredString(g, "Checks", new Rectangle(0,0,width/2,3*height/4), g.getFont().deriveFont(g.getFont().getSize() * 5.0F))
            g.drawImage(introImage,0,0,width,height,null)
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
                if (Some(i,j) == selectedCase)
                {
                    // Selected case : yellow
                    g.setColor(Color.yellow);
                    g.fillRect(i*width/8,j*height/8,width/8,height/8);
                }
                // Coloring cases we can move to : green
                g.setColor(Color.green);
                selectedCase match
                {
                    case None => {}
                    case Some ((x,y)) if game.canMove(x,y,i,j) => g.fillRect(i*width/8,j*height/8,width/8,height/8);
                    case _ => {}
                }
            }
        }

        // Draw the grid
        g.setStroke(new BasicStroke(5));
        g.setColor(new Color(200,200,200));
        for (i<-0 to 8)
        {
            g.drawLine(0,i*height/8,width,i*height/8);
            g.drawLine(i*width/8,0,i*width/8,height);
        }

        // Draw each piece in game
        for (i<-0 to 7)
        {
            for (j<-0 to 7)
            {
                val piece = game.pieceAtPosition(i,j)
                if (piece != null)
                    g.drawImage(piece.getImage,i*width/8,j*height/8,width/8,height/8,null)
            }
        }

        // Print message !
        if (message != null)
        {
            g.setColor(Color.red)
            drawCenteredString(g, message, new Rectangle(0,0,width,height), g.getFont().deriveFont(g.getFont().getSize() * 3.0F))
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
                    if (selectedCase == Some(x,y))
                        selectedCase = None
                    else
                    {
                        if (game.canMove(x,y))
                            selectedCase = Some(x,y)
                        else if (selectedCase != None)
                        {
                            // Play the move if possible
                            val Some ((sel_x, sel_y)) = selectedCase
                            if (game.canMove(sel_x, sel_y, x, y))
                            {
                                selectedCase = None
                                canPlay = false
                                game.move(sel_x, sel_y, x, y)
                            }
                        }
                    }
                    repaint
                }
            }
        }
        case _ => {}
    }

}