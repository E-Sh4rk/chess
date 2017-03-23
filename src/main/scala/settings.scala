
import scala.swing.GridBagPanel
import scala.swing.CheckBox
import scala.swing.ToggleButton
import scala.swing.ButtonGroup
import scala.swing.TextArea
import scala.swing.Label
import java.awt.Dimension

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

    private val label : Label = new Label("Settings below only apply to a new game :")

    private val clock : CheckBox = new CheckBox("Activate game clock")
    private val clockText : TextArea = new TextArea("3600 0 5\nOnly one period is supported for now...",5,25)

    private val c = new Constraints
    c.ipady = 25; c.ipadx = 25
    c.gridx = 1; c.gridy = 0
    add(new Label, c)
    
    c.gridx = 0; c.gridy = 0
    add(p1_human, c)
    c.gridx = 2; c.gridy = 0
    add(p1_ai, c)

    c.gridx = 1; c.gridy = 1
    add(new Label, c)

    c.gridx = 0; c.gridy = 2
    add(p2_human, c)
    c.gridx = 2; c.gridy = 2
    add(p2_ai, c)

    c.gridwidth = 3; c.ipady = 100
    c.gridx = 0; c.gridy = 3
    add(label, c)

    // TODO : Settings for game mode (variant)

    c.gridwidth = 3; c.ipady = 25
    c.gridx = 0; c.gridy = 4
    add(clock, c)

    c.gridx = 0; c.gridy = 5
    add(clockText, c)

    def white_is_human () : Boolean = { lastWhite = p1_human.selected ; return p1_human.selected }
    def black_is_human () : Boolean = { lastBlack = p2_human.selected ; return p2_human.selected }
    private var lastWhite = p1_human.selected
    def white_player_has_changed () : Boolean = { return lastWhite != p1_human.selected }
    private var lastBlack = p2_human.selected
    def black_player_has_changed () : Boolean = { return lastBlack != p2_human.selected }

    def clockEnabled () : Boolean = { return clock.selected }
    def clockSettings () : String = { return clockText.text }
}