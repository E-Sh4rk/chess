import swing._
import swing.event._
import java.awt.Dimension

object MyApp extends SimpleSwingApplication {
	def top = new MainFrame {

		title = "Checks"

        val canvas = new Canvas(1000,750)
        var game:Game = null
        var explore_mode = false
        var currentSimulatedPlayer:SimulatedPlayer = null

        // Game Buttons
        val newGame = new Button("New Game")
        val loadGame = new Button("Load Game")
        val saveGame = new Button("Save Game")
        val settings = new Button("Settings")
        val prev_final = new Button("<=")
        val prev = new Button("<-")
        val next = new Button("->")
        val next_final = new Button("=>")
        val switch_mode = new Button("Switch to explore mode")
        val buttons1 = new FlowPanel
        {
            contents += newGame
            contents += loadGame
            contents += saveGame
            contents += settings
        }
        val buttons2 = new FlowPanel
        {
            contents += prev_final
            contents += prev
            contents += next
            contents += next_final
            contents += switch_mode
        }
        exploreButtonsSetEnabled(false)
        val buttons = new BorderPanel
        {
            add (buttons1, BorderPanel.Position.South)
            add (buttons2, BorderPanel.Position.North)
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
                    loadGame.visible = false
                    saveGame.visible = false
                    buttons2.visible = false
                }
                else
                {
                    settings.text = "Settings"
                    newGame.visible = true
                    loadGame.visible = true
                    saveGame.visible = true
                    buttons2.visible = true
                }
            }
            def settingsDisplayed() : Boolean = { settingsPanel.visible }
        }
        contents = content

        // Utilitary functions
        private def newWhitePlayer () : Player =
        {
            if (explore_mode)
                return currentSimulatedPlayer
            if (settingsPanel.white_is_human)
                return canvas
            else
                return new PrimitiveAI
        }
        private def newBlackPlayer () : Player =
        {
            if (explore_mode)
                return currentSimulatedPlayer
            if (settingsPanel.black_is_human)
                return canvas
            else
                return new PrimitiveAI
        }
        private def clockSettings () : TimePeriod =
        {
            if (settingsPanel.clockEnabled)
            {
                var time = 0 ; var rounds = 0 ; var inc = 0
                val lines = settingsPanel.clockSettings.split("[\\r\\n]+")
                for (line <- lines)
                {
                    try
                    {
                        val values = line.split("\\s+")
                        time = values(0).toInt
                        rounds = values(1).toInt
                        inc = values(2).toInt
                        return new TimePeriod(time, rounds, inc)
                    }
                    catch { case e:Exception => {} }
                }
            }
            return new TimePeriod(-1, 0, 0)
        }
        private def switchToExploreMode() =
        {
            if (game != null)
            {
                explore_mode = true
                currentSimulatedPlayer = new SimulatedPlayer(game.getHistory)
                switch_mode.text = "Switch to play mode"
                exploreButtonsSetEnabled(true)
            }
        }
        private def switchToPlayMode() =
        {
            explore_mode = false
            currentSimulatedPlayer = null
            switch_mode.text = "Switch to explore mode"
            exploreButtonsSetEnabled(false)
        }
        private def exploreButtonsSetEnabled(b:Boolean) =
        {
            prev_final.enabled = b
            prev.enabled = b
            next.enabled = b
            next_final.enabled = b
        }

        // Reactions
        listenTo(newGame, settings, this, switch_mode)
        reactions += {
            case ButtonClicked (source) =>
            {
                if (source == newGame)
                {
                    if (game != null) game.suspend
                    switchToPlayMode
                    game = new Game(canvas, newWhitePlayer, newBlackPlayer, settingsPanel.gameMode, clockSettings)
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
                if (source == switch_mode)
                {
                    if (!explore_mode)
                        switchToExploreMode
                    else
                        switchToPlayMode
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
