
import scala.swing.GridBagPanel
import scala.swing.CheckBox
import scala.swing.ToggleButton
import scala.swing.ButtonGroup
import scala.swing.TextArea
import scala.swing.ComboBox
import scala.swing.Label
import java.awt.Dimension
import swing.event._

object AI_Type extends Enumeration {
    type AI_Type = Value
    val Random, AlphaBeta, GNU_Chess = Value
}

/**
A panel for the settings (players, game mode, clock...)
*/
class Settings extends GridBagPanel
{
    private val p1_ai : /*CheckBox*/ToggleButton = new /*CheckBox*/ToggleButton ("White player is AI")
    private val p1_human : /*CheckBox*/ToggleButton = new /*CheckBox*/ToggleButton ("White player is human")
    p1_human.selected = true
    private val player1 : ButtonGroup = new ButtonGroup(p1_human, p1_ai)
    private val p2_ai : /*CheckBox*/ToggleButton = new /*CheckBox*/ToggleButton ("Black player is AI")
    private val p2_human : /*CheckBox*/ToggleButton = new /*CheckBox*/ToggleButton ("Black player is human")
    p2_human.selected = true
    private val player2 : ButtonGroup = new ButtonGroup(p2_human, p2_ai)

    private val labelAI1 : Label = new Label("AI type (White Player)")
    private val aiType1 : ComboBox[String] = new ComboBox[String](List("Random","Alpha-Beta","GNU Chess"))
    private val labelAI2 : Label = new Label("AI type (Black Player)")
    private val aiType2 : ComboBox[String] = new ComboBox[String](List("Random","Alpha-Beta","GNU Chess"))


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
    
    // White player
    c.ipady = 10
    c.gridx = 0; c.gridy = 0
    add(p1_human, c)
    c.gridx = 2; c.gridy = 0
    add(p1_ai, c)

    c.gridx = 1; c.gridy = 1
    add(new Label, c)

    // Black player
    c.gridx = 0; c.gridy = 2
    add(p2_human, c)
    c.gridx = 2; c.gridy = 2
    add(p2_ai, c)

    c.gridx = 1; c.gridy = 3
    add(new Label, c)

    // AI1
    c.gridx = 0; c.gridy = 4
    add(labelAI1, c)
    c.gridwidth = 1; c.ipady = 0
    c.gridx = 2; c.gridy = 4
    add(aiType1, c)

    // AI2
    c.gridx = 0; c.gridy = 5
    add(labelAI2, c)
    c.gridwidth = 1; c.ipady = 0
    c.gridx = 2; c.gridy = 5
    add(aiType2, c)

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

    def checkWhite () : Unit =
    {
        lastWhite = p1_human.selected
        lastWhiteAiType = aiType1.selection.item
    }
    def checkBlack () : Unit =
    {
        lastBlack = p2_human.selected
        lastBlackAiType = aiType2.selection.item
    }
    def white_is_human () : Boolean = { return p1_human.selected }
    def black_is_human () : Boolean = { return p2_human.selected }
    def whiteAiType () : AI_Type.AI_Type =
    {
        return aiType1.selection.item match
        {
            case "Random" => AI_Type.Random
            case "Alpha-Beta" => AI_Type.AlphaBeta
            case "GNU Chess" => AI_Type.GNU_Chess
            case _ => AI_Type.Random
        }
    }
    def blackAiType () : AI_Type.AI_Type =
    {
        return aiType2.selection.item match
        {
            case "Random" => AI_Type.Random
            case "Alpha-Beta" => AI_Type.AlphaBeta
            case "GNU Chess" => AI_Type.GNU_Chess
            case _ => AI_Type.Random
        }
    }

    private var lastWhiteAiType = aiType1.selection.item
    private var lastWhite = p1_human.selected
    def white_player_has_changed () : Boolean = { return lastWhite != p1_human.selected || (p1_ai.selected && lastWhiteAiType != aiType1.selection.item) }
    private var lastBlackAiType = aiType2.selection.item
    private var lastBlack = p2_human.selected
    def black_player_has_changed () : Boolean = { return lastBlack != p2_human.selected || (p2_ai.selected && lastBlackAiType != aiType2.selection.item)  }

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