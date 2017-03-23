import swing._
import swing.event._
import java.awt.Dimension

object MyApp extends SimpleSwingApplication {
	def top = new MainFrame {

		title = "Checks"

        val canvas = new Canvas(1000,750)
        var game:Game = null

        // Game Buttons
        val newGame = new Button("New Game")
        val settings = new Button("Settings")
        val buttons = new FlowPanel
        {
            contents += newGame
            contents += settings
        }

        // Settings
        val settingsPanel = new Settings

        // Main Content
        val content = new BorderPanel
        {
            settingsPanel.visible = false
            canvas.visible = true
            add (canvas, BorderPanel.Position.North)
            add (settingsPanel, BorderPanel.Position.Center)
            add (buttons, BorderPanel.Position.South)
            def switch() : Unit =
            {
                settingsPanel.visible = !settingsPanel.visible
                canvas.visible = !settingsPanel.visible
                if (settingsPanel.visible)
                {
                    settings.text = "Go back to the game"
                    newGame.visible = false
                }
                else
                {
                    settings.text = "Settings"
                    newGame.visible = true
                }
            }
            def settingsDisplayed() : Boolean = { settingsPanel.visible }
        }
        contents = content

        // Utilitary functions
        private def newWhitePlayer () : Player =
        {
            if (settingsPanel.white_is_human)
                return canvas
            else
                return new PrimitiveAI
        }
        private def newBlackPlayer () : Player =
        {
            if (settingsPanel.black_is_human)
                return canvas
            else
                return new PrimitiveAI
        }

        // Reactions
        listenTo(newGame, settings, this)
        reactions += {
            case ButtonClicked (source) =>
            {
                if (source == newGame)
                {
                    if (game != null) game.suspend
                    game = new Game(canvas, newWhitePlayer, newBlackPlayer)
                }
                if (source == settings)
                {
                    if (game != null && !content.settingsDisplayed) game.suspend
                    content.switch
                    if (game != null && !content.settingsDisplayed)
                    {
                        if (settingsPanel.white_player_has_changed)
                            game.setWhitePlayer(newWhitePlayer)
                        if (settingsPanel.black_player_has_changed)
                            game.setBlackPlayer(newBlackPlayer)
                        game.resume
                    }
                }
            }
            case WindowClosing(_) => { if (game != null) game.suspend }
            case UIElementResized (source) =>
            {
                if (this == source)
                {
                    content.revalidate
                    if (canvas != null)
                        canvas.resize(content.size.width, content.size.height-buttons.size.height);
                }
            }
        }
	}
}
