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
    private object InterfaceStatus extends Enumeration {
        type InterfaceStatus = Value
        val Default, ConfirmResign, ConfirmDraw = Value
    }

    private var game : Game = null
    private var canPlay : Boolean = false
    private var message : String = null
    private var selectedCase : Option[(Int,Int)] = None
    private var selectedCase2 : Option[(Int,Int)] = None // Used for promotion
    private var drawAfterMove : Boolean = false
    private var interfaceStatus : InterfaceStatus.InterfaceStatus = InterfaceStatus.Default

    /**
    Initializes the canvas for a new game.
    */
    def newGame (g:Game) : Unit =
    {
        game = g
        canPlay = false
        message = null
        selectedCase = None
        selectedCase2 = None
        drawAfterMove = false
        interfaceStatus = InterfaceStatus.Default
        resize(this.size.width, this.size.height)
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
    private var panel_width = 0
    this.preferredSize = new Dimension(width, height)
    /**
    Resize the canvas.
    */
    def resize(w:Int, h:Int)
    {
        width = w ; height = h
        if (height <= width*7/8 && game != null)
        {
            panel_width = w/4
            if (panel_width > 250)
                panel_width = 250
            width = w-panel_width
        }
        this.preferredSize = new Dimension(width, height)
        repaint
    }

    private def countRemainingPieces(team:Round.Round) : Array[Int] =
    {
        val res : Array[Int] = Array(0,0,0,0,0)
        for (i<-0 to game.dim_x-1)
        {
            for (j<-0 to game.dim_y-1)
            {
                val p = game.pieceAtPosition(i,j)
                if (p != null)
                {
                    if (p.team == team)
                    {
                        p.pieceType match
                        {
                            case PieceType.Pawn => res(0)+=1
                            case PieceType.Rook => res(1)+=1
                            case PieceType.Knight => res(2)+=1
                            case PieceType.Bishop => res(3)+=1
                            case PieceType.Queen => res(4)+=1
                            case _ => { }
                        }
                    }
                }
            }
        }
        return res
    }

    private val introImage = ImageIO.read(new File("img/Chess_intro.png"))
    private val promoQueen = ImageIO.read(new File("img/tux_queen.png"))
    private val promoKnight = ImageIO.read(new File("img/tux_knight.png"))
    private val promoBishop = ImageIO.read(new File("img/tux_bishop.png"))
    private val promoRook = ImageIO.read(new File("img/tux_rook.png"))
    private val whiteTrait = ImageIO.read(new File("img/w_king.png"))
    private val blackTrait = ImageIO.read(new File("img/b_king.png"))
    private val w_queen = ImageIO.read(new File("img/w_queen.png"))
    private val b_queen = ImageIO.read(new File("img/b_queen.png"))
    private val w_bishop = ImageIO.read(new File("img/w_bishop.png"))
    private val b_bishop = ImageIO.read(new File("img/b_bishop.png"))
    private val w_knight = ImageIO.read(new File("img/w_knight.png"))
    private val b_knight = ImageIO.read(new File("img/b_knight.png"))
    private val w_rook = ImageIO.read(new File("img/w_rook.png"))
    private val b_rook = ImageIO.read(new File("img/b_rook.png"))
    private val w_pawn = ImageIO.read(new File("img/w_pawn.png"))
    private val b_pawn = ImageIO.read(new File("img/b_pawn.png"))
    private val r_button = ImageIO.read(new File("img/Resign.png"))
    private val resign_button = ImageIO.read(new File("img/Resign_full.png"))
    private val d_button = ImageIO.read(new File("img/Draw.png"))
    private val draw_button = ImageIO.read(new File("img/Draw_full.png"))

    private var button_r : Rectangle = null
    private var button_d : Rectangle = null
    private var button_resign : Rectangle = null
    private var button_draw : Rectangle = null
    override def paintComponent(g: Graphics2D) : Unit = {
        super.paintComponent(g)

        button_r = null
        button_d = null
        button_resign = null
        button_draw = null

        if (game == null)
        {
            // Drawing the intro image
            g.setColor(Color.black)
            if (width*2 <= height*3)
                drawCenteredString(g, "Checks", new Rectangle(0,0,width,height-width/2), g.getFont().deriveFont(25F * width / 100F))
            g.drawImage(introImage,0,height-width/2,width,width/2,null)
            return
        }

        // DRAWING THE IG INTERFACE

        if (panel_width > 0)
        {
            // Round info display
            g.setColor(Color.black)
            if (game.getRound == Round.White)
                g.drawImage(whiteTrait,width+0*panel_width/5,height/2-panel_width/10,panel_width/5,panel_width/5,null)
            if (game.getRound == Round.Black)
                g.drawImage(blackTrait,width+0*panel_width/5,height/2-panel_width/10,panel_width/5,panel_width/5,null)
            drawCenteredString(g, game.getRoundNumber.toString, new Rectangle(width+1*panel_width/5,0,panel_width/5,height),
            g.getFont().deriveFont(10F * panel_width / 100F))
            drawCenteredString(g, "("+game.getThreefoldRepetitionCounter.toString+"/3, "+game.getFiftyMoveRuleCounter.toString+"/50)", new Rectangle(width+3*panel_width/5,0,panel_width/5,height),
            g.getFont().deriveFont(7F * panel_width / 100F))
            // Clock
            drawCenteredString(g, game.getClock(Round.Black).toString, new Rectangle(width+2*panel_width/5,height/2-panel_width/2,panel_width/5,panel_width/2),
            g.getFont().deriveFont(10F * panel_width / 100F))
            drawCenteredString(g, game.getClock(Round.White).toString, new Rectangle(width+2*panel_width/5,height/2,panel_width/5,panel_width/2),
            g.getFont().deriveFont(10F * panel_width / 100F))
            // Piece counter
            val whiteIcons = Array(w_pawn,w_rook,w_knight,w_bishop,w_queen)
            val whiteCount = countRemainingPieces(Round.White)
            val blackIcons = Array(b_pawn,b_rook,b_knight,b_bishop,b_queen)
            val blackCount = countRemainingPieces(Round.Black)
            for (i<-0 to 4)
            {
                drawCenteredString(g, whiteCount(i).toString, new Rectangle(width+i*panel_width/5,height-2*panel_width/5,panel_width/5,panel_width/5),
                g.getFont().deriveFont(10F * panel_width / 100F))
                g.drawImage(whiteIcons(i),width+i*panel_width/5,height-1*panel_width/5,panel_width/5,panel_width/5,null)
                drawCenteredString(g, blackCount(i).toString, new Rectangle(width+i*panel_width/5,panel_width/5,panel_width/5,panel_width/5),
                g.getFont().deriveFont(10F * panel_width / 100F))
                g.drawImage(blackIcons(i),width+i*panel_width/5,0,panel_width/5,panel_width/5,null)
            }
            // Buttons
            if (canPlay && game.getRound != Round.Finished)
            {
                var buttons_y = height/2 - panel_width/10
                var label_y = buttons_y
                if (game.getRound == Round.White)
                {
                    buttons_y = 3*height/4 - panel_width/10
                    label_y = buttons_y-panel_width/5
                }
                if (game.getRound == Round.Black)
                {
                    buttons_y = 1*height/4 - panel_width/10
                    label_y = buttons_y+panel_width/5
                }
                if (game.canRequestDraw)
                    drawCenteredString(g, "You can request a draw immediatly.", new Rectangle(width,label_y,panel_width,panel_width/5),
                    g.getFont().deriveFont(5F * panel_width / 100F))
                if (interfaceStatus == InterfaceStatus.Default)
                {
                    if (!drawAfterMove)
                    {
                        button_r = new Rectangle(width+1*panel_width/5,buttons_y,panel_width/5,panel_width/5)
                        button_d = new Rectangle(width+3*panel_width/5,buttons_y,panel_width/5,panel_width/5)
                        g.drawImage(r_button,button_r.x,button_r.y,button_r.width,button_r.height,null)
                        g.drawImage(d_button,button_d.x,button_d.y,button_d.width,button_d.height,null)
                    }
                    else
                    {
                        drawCenteredString(g, "You have requested a draw.", new Rectangle(width,buttons_y,panel_width,panel_width/5),
                        g.getFont().deriveFont(5F * panel_width / 100F))
                    }
                }
                if (interfaceStatus == InterfaceStatus.ConfirmResign)
                {
                    button_resign = new Rectangle(width+3*panel_width/10,buttons_y,2*panel_width/5,panel_width/5)
                    g.drawImage(resign_button,button_resign.x,button_resign.y,button_resign.width,button_resign.height,null)
                }
                if (interfaceStatus == InterfaceStatus.ConfirmDraw)
                {
                    button_draw = new Rectangle(width+3*panel_width/10,buttons_y,2*panel_width/5,panel_width/5)
                    g.drawImage(draw_button,button_draw.x,button_draw.y,button_draw.width,button_draw.height,null)
                }
            }
        }

        // DRAWING THE GAME

        if (selectedCase2 != None)
        {
            // Promotion !
            val max_height = math.min(height,width*3/2)
            val max_width = max_height*2/3
            g.drawImage(promoQueen,(width-(max_width/2))/2,0,max_width/2,max_height/2,null)
            g.drawImage(promoRook,(width/3-(max_width/3))/2,height/2,max_width/3,max_height/3,null)
            g.drawImage(promoKnight,width/3+(width/3-(max_width/3))/2,height/2,max_width/3,max_height/3,null)
            g.drawImage(promoBishop,2*width/3+(width/3-(max_width/3))/2,height/2,max_width/3,max_height/3,null)
            return
        }
        
        // Coloring the board
        val possibleMoves = game.possibleMoves
        for (i<-0 to game.dim_x-1)
        {
            for (j<-0 to game.dim_y-1)
            {
                if ((i+j) % 2 == 0)
                {
                    // White case
                    g.setColor(Color.white);
                    g.fillRect(i*width/game.dim_x,j*height/game.dim_y,width/game.dim_x,height/game.dim_y);
                }
                else
                {
                    // 'Black' case
                    g.setColor(new Color(220,220,220));
                    g.fillRect(i*width/game.dim_x,j*height/game.dim_y,width/game.dim_x,height/game.dim_y);
                }
                if (Some(i,j) == selectedCase)
                {
                    // Selected case : yellow
                    g.setColor(/*Color.yellow*/new Color(0xFF,0xFF,0x66));
                    g.fillRect(i*width/game.dim_x,j*height/game.dim_y,width/game.dim_x,height/game.dim_y);
                }
                // Coloring cases we can move to : green
                g.setColor(/*Color.green*/new Color(0x62,0xDD,0x62));
                selectedCase match
                {
                    case None => {}
                    case Some ((x,y)) if possibleMoves contains (x,y,i,j) => g.fillRect(i*width/game.dim_x,j*height/game.dim_y,width/game.dim_x,height/game.dim_y);
                    case _ => {}
                }
                // Drawing the piece
                val piece = game.pieceAtPosition(i,j)
                val min = math.min(width/game.dim_x,height/game.dim_y)
                if (piece != null)
                    g.drawImage(piece.getImage,i*width/game.dim_x+(width/game.dim_x-min)/2,j*height/game.dim_y+(height/game.dim_y-min)/2,min,min,null)
            }
        }

        // Drawing the grid
        g.setColor(new Color(100,100,100));
        g.setStroke(new BasicStroke(2));
        for (i<-0 to game.dim_x)
            g.drawLine(i*width/game.dim_x,0,i*width/game.dim_x,height);
        for (j<-0 to game.dim_y)
            g.drawLine(0,j*height/game.dim_y,width,j*height/game.dim_y);

        // Prints a message !
        if (message != null)
        {
            g.setColor(Color.red)
            drawCenteredString(g, message, new Rectangle(0,0,width,height), g.getFont().deriveFont(7F * width / 100F))
        }
    }
    private def drawCenteredString(g:Graphics, text:String, rect:Rectangle, font:Font)
    {
        val metrics:FontMetrics = g.getFontMetrics(font);
        val x:Int = rect.x + (rect.width - metrics.stringWidth(text)) / 2;
        val y:Int = rect.y + ((rect.height - metrics.getHeight()) / 2) + metrics.getAscent();
        g.setFont(font);
        g.drawString(text, x, y);
    }

    // Events (input)
    listenTo(mouse.clicks)
    private def hasPlayed() : Unit =
    {
        selectedCase = None
        selectedCase2 = None
        drawAfterMove = false
        canPlay = false
    }
    reactions +=
    {
        case MouseClicked(src, pt, mod, clicks, pops) => {
            if (game != null && canPlay)
            {
                interfaceStatus = InterfaceStatus.Default
                if (pt.x >= 0 && pt.y >= 0 && pt.x < width && pt.y < height)
                {
                    val x = pt.x * game.dim_x / width
                    val y = pt.y * game.dim_y / height

                    if (selectedCase2 != None)
                    {
                        var ptype = PieceType.Unknown
                        // Promotion ! Choosing a piece type
                        if (pt.y <= height/2)
                            ptype = PieceType.Queen
                        else if (pt.x <= width/3)
                            ptype = PieceType.Rook
                        else if (pt.x <= 2*width/3)
                            ptype = PieceType.Knight
                        else if (pt.x <= width)
                            ptype = PieceType.Bishop
                        // Do the promotion move
                        val Some ((sel_x, sel_y)) = selectedCase
                        val Some ((sel_x2, sel_y2)) = selectedCase2
                        val dam = drawAfterMove
                        hasPlayed
                        game.move(sel_x, sel_y, sel_x2, sel_y2, ptype, dam)
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
                                if (game.pieceAtPosition(sel_x,sel_y).pieceType == PieceType.Pawn && (y == 0 || y == game.dim_y-1))
                                    // Promotion move
                                    selectedCase2 = Some(x, y)
                                else
                                {
                                    // Not a promotion move
                                    val dam = drawAfterMove
                                    hasPlayed
                                    game.move(sel_x, sel_y, x, y, PieceType.Unknown, dam)
                                } 
                            }
                        }
                    }
                }
                else if (pt.x >= width && pt.y >= 0 && pt.x < width+panel_width && pt.y < height)
                {
                    if (button_d != null)
                        if (button_d.contains(pt.x,pt.y))
                            interfaceStatus = InterfaceStatus.ConfirmDraw
                    if (button_r != null)
                        if (button_r.contains(pt.x,pt.y))
                            interfaceStatus = InterfaceStatus.ConfirmResign
                    if (button_draw != null)
                        if (button_draw.contains(pt.x,pt.y))
                        {
                            if (game.canRequestDraw)
                            {
                                hasPlayed
                                game.requestDraw
                            }
                            else
                                drawAfterMove = true
                        } 
                    if (button_resign != null)
                        if (button_resign.contains(pt.x,pt.y))
                        {
                            hasPlayed
                            game.resign
                        }   
                }
                repaint
            }
        }
    }

}
