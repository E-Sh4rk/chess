
import scala.swing.GridBagPanel
import scala.swing.CheckBox
import scala.swing.ToggleButton
import scala.swing.ButtonGroup
import scala.swing.TextArea
import scala.swing.ComboBox
import scala.swing.Label
import java.awt.Dimension
import swing.event._

object Player_Type extends Enumeration {
    type Player_Type = Value
    val Human, PrimitiveAI, AlphaBeta, GNU_Chess = Value
}

/**
A panel for the settings (players, game mode, clock...)
*/
class Settings extends GridBagPanel
{
    private val labelP1 : Label = new Label("White Player")
    private val pType1 : ComboBox[String] = new ComboBox[String](List("Local Human","Primitive AI","Alpha-Beta","GNU Chess"))
    private val labelP2 : Label = new Label("Black Player")
    private val pType2 : ComboBox[String] = new ComboBox[String](List("Local Human","Primitive AI","Alpha-Beta","GNU Chess"))


    private val label : Label = new Label("Settings below only apply to a new game :")

    private val clock : CheckBox = new CheckBox("Activate game clock")
    private val clockText : TextArea = new TextArea("#SECONDS NB_OF_ROUNDS INCREMENT\n3600 0 5",5,25)
    clockText.enabled = false

    private val labelVariant : Label = new Label("Game mode (variant)")
    private val variant : ComboBox[String] = new ComboBox[String](List("Vanilla","Janus","Capablanca"))

    private val c = new Constraints
    c.ipady = 25; c.ipadx = 25
    c.gridx = 1; c.gridy = 0
    add(new Label, c)
    
    // P1
    c.gridx = 0; c.gridy = 4
    add(labelP1, c)
    c.gridwidth = 1; c.ipady = 0
    c.gridx = 2; c.gridy = 4
    add(pType1, c)

    // P2
    c.gridx = 0; c.gridy = 5
    add(labelP2, c)
    c.gridwidth = 1; c.ipady = 0
    c.gridx = 2; c.gridy = 5
    add(pType2, c)

    // Label
    c.gridwidth = 3; c.ipady = 100
    c.gridx = 0; c.gridy = 6
    add(label, c)

    // Variant
    c.gridwidth = 1; c.ipady = 25
    c.gridx = 0; c.gridy = 7
    add(labelVariant, c)
    c.gridwidth = 1; c.ipady = 0
    c.gridx = 2; c.gridy = 7
    add(variant, c)

    // Clock
    c.gridwidth = 3; c.ipady = 25
    c.gridx = 0; c.gridy = 8
    add(clock, c)
    c.gridx = 0; c.gridy = 9
    add(clockText, c)

    def whitePlayerType () : Player_Type.Player_Type =
    {
        return pType1.selection.item match
        {
            case "Primitive AI" => Player_Type.PrimitiveAI
            case "Alpha-Beta" => Player_Type.AlphaBeta
            case "GNU Chess" => Player_Type.GNU_Chess
            case _ => Player_Type.Human
        }
    }
    def blackPlayerType () : Player_Type.Player_Type =
    {
        return pType2.selection.item match
        {
            case "Primitive AI" => Player_Type.PrimitiveAI
            case "Alpha-Beta" => Player_Type.AlphaBeta
            case "GNU Chess" => Player_Type.GNU_Chess
            case _ => Player_Type.Human
        }
    }

    def clockEnabled () : Boolean = { return clock.selected }
    def clockSettings () : String = { return clockText.text }

    def gameMode () : GameMode.GameMode =
    {
        return variant.selection.item match
        {
            case "Vanilla" => GameMode.Vanilla
            case "Janus" => GameMode.Janus
            case "Capablanca" => GameMode.Capablanca
            case _ => GameMode.Vanilla
        }
    }

    // Reactions
    listenTo(clock)
    reactions +=
    {
        case ButtonClicked (source) =>
        {
            if (source == clock)
            {
                if (clock.selected)
                    clockText.enabled = true
                else
                    clockText.enabled = false
            }
        }
    }
}
