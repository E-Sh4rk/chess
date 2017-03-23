
import scala.swing.GridBagPanel
import scala.swing.CheckBox
import scala.swing.ButtonGroup
import scala.swing.Label

class Settings extends GridBagPanel
{
    // TODO : Settings for clock
    private val p1_ai : CheckBox = new CheckBox ("White player is AI")
    private val p1_human : CheckBox = new CheckBox ("White player is human")
    private val player1 : ButtonGroup = new ButtonGroup(p1_human, p1_ai)
    private val p2_ai : CheckBox = new CheckBox ("Black player is AI")
    private val p2_human : CheckBox = new CheckBox ("Black player is human")
    private val player2 : ButtonGroup = new ButtonGroup(p2_human, p2_ai)

    private val label : Label = new Label("Settings below only apply to a new game :")

    private val c = new Constraints
    
    p1_human.selected = true
    c.gridx = 0; c.gridy = 0
    add(p1_human, c)
    c.gridx = 1; c.gridy = 0
    add(p1_ai, c)

    p2_human.selected = true
    c.gridx = 0; c.gridy = 1
    add(p2_human, c)
    c.gridx = 1; c.gridy = 1
    add(p2_ai, c)

    c.gridwidth = 2; c.ipady = 100
    c.gridx = 0; c.gridy = 2
    add(label, c)

    def white_is_human () : Boolean = { lastWhite = p1_human.selected ; return p1_human.selected }
    def black_is_human () : Boolean = { lastBlack = p2_human.selected ; return p2_human.selected }
    private var lastWhite = p1_human.selected
    def white_player_has_changed () : Boolean = { return lastWhite != p1_human.selected }
    private var lastBlack = p2_human.selected
    def black_player_has_changed () : Boolean = { return lastBlack != p2_human.selected }
}