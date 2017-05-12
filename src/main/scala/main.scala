import swing._
import swing.event._
import java.awt.Dimension
import java.io.File
import javax.swing.filechooser.FileFilter

object MyApp extends SimpleSwingApplication {
	def top = new MainFrame {

		title = "Checks"

        val canvas = new Canvas(1000,750)
        var game:Game = null
        var explore_mode = false
        var currentSimulatedPlayer:SimulatedPlayer = null

        // File chooser
        val pgn_ff : FileFilter = new FileFilter() {
            override def getDescription() : String = { return "Portable Game Notation (*.pgn)" }
            override def accept(f:File) : Boolean =
            {
                if (f.isDirectory)
                    return true
                else
                {
                    var filename = f.getName.toLowerCase
                    return filename.endsWith(".pgn")
                }
            }
        }
        val chooser = new FileChooser
        chooser.multiSelectionEnabled = false
        chooser.fileFilter = pgn_ff
        chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly

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
        switch_mode.enabled = false
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
        var current_cecp:CECP_AI = null
        private def newWhitePlayer () : Player =
        {
            // Avoid multiple instance of gnuchess : it causes issues
            if (!explore_mode && settingsPanel.whitePlayerType == Player_Type.GNU_Chess && settingsPanel.blackPlayerType == Player_Type.GNU_Chess)
            {
                if (current_cecp == null)
                    current_cecp = new CECP_AI(settingsPanel.gnuChessTimeLimit)
                current_cecp.max_think_time = settingsPanel.gnuChessTimeLimit
                return current_cecp
            }
            else
                current_cecp = null

            if (explore_mode)
                return currentSimulatedPlayer
            if (settingsPanel.whitePlayerType == Player_Type.Human)
                return canvas
            else
            {
                settingsPanel.whitePlayerType match
                {
                    case Player_Type.GNU_Chess => return new CECP_AI(settingsPanel.gnuChessTimeLimit)
                    case Player_Type.AlphaBeta => return new AlphaBetaAI(new EvalToy(), settingsPanel.alphaBetaDepth)
                    case _ => return new PrimitiveAI
                }
            }
        }
        private def newBlackPlayer () : Player =
        {
            // Avoid multiple instance of gnuchess : it causes issues
            if (!explore_mode && settingsPanel.whitePlayerType == Player_Type.GNU_Chess && settingsPanel.blackPlayerType == Player_Type.GNU_Chess)
            {
                if (current_cecp == null)
                    current_cecp = new CECP_AI(settingsPanel.gnuChessTimeLimit)
                current_cecp.max_think_time = settingsPanel.gnuChessTimeLimit
                return current_cecp
            }
            else
                current_cecp = null

            if (explore_mode)
                return currentSimulatedPlayer
            if (settingsPanel.blackPlayerType == Player_Type.Human)
                return canvas
            else
            {
                settingsPanel.blackPlayerType match
                {
                    case Player_Type.GNU_Chess => return new CECP_AI(settingsPanel.gnuChessTimeLimit)
                    case Player_Type.AlphaBeta => return new AlphaBetaAI(new EvalToy(), settingsPanel.alphaBetaDepth)
                    case _ => return new PrimitiveAI
                }
            }
        }
        private def clockSettings () : Array[TimePeriod] =
        {
            val res = new scala.collection.mutable.MutableList[TimePeriod]()
            if (settingsPanel.clockEnabled)
            {
                var time = 0 ; var rounds = 0 ; var inc = 0
                val lines = settingsPanel.clockSettings.split("[\\r\\n]+")
                for (line <- lines)
                {
                    try
                    {
                        if (!line.isEmpty)
                        {
                            if (line(0) != '#')
                            {
                                val values = line.split("\\s+")
                                time = values(0).toInt
                                rounds = values(1).toInt
                                inc = values(2).toInt
                                res += new TimePeriod(time, rounds, inc)
                            }
                        }
                    }
                    catch { case e:Exception => {} }
                }
            }
            return res.toArray
        }
        private def switchToExploreMode() =
        {
            if (game != null)
            {
                explore_mode = true
                currentSimulatedPlayer = new SimulatedPlayer(game.getHistory, canvas)
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
            switch_mode.enabled = true
        }

        // Reactions
        listenTo(newGame, settings, saveGame, loadGame, this, switch_mode, next, next_final, prev, prev_final)
        reactions += {
            case ButtonClicked (source) =>
            {
                if (source == newGame)
                {
                    if (game != null) game.suspend
                    switchToPlayMode
                    prev.enabled = false ; prev_final.enabled = false
                    game = new Game(canvas, newWhitePlayer, newBlackPlayer, settingsPanel.gameMode, clockSettings)
                }
                if (source == settings)
                {
                    if (game != null && !content.settingsDisplayed) game.suspend
                    content.switch
                    if (game != null && !content.settingsDisplayed)
                    {
                        game.setWhitePlayer(newWhitePlayer)
                        game.setBlackPlayer(newBlackPlayer)
                        game.resume
                    }
                }
                if (source == switch_mode)
                {
                    if (game != null)
                    {
                        if (!explore_mode)
                        {
                            
                            switchToExploreMode
                            next.enabled = false ; next_final.enabled = false
                            game.suspend
                            game.setWhitePlayer(newWhitePlayer)
                            game.setBlackPlayer(newBlackPlayer)
                            game.resume
                        }
                        else
                        {
                            switchToPlayMode
                            game.suspend
                            game.setWhitePlayer(newWhitePlayer)
                            game.setBlackPlayer(newBlackPlayer)
                            game.resume
                        }
                    }
                }
                if (source == saveGame)
                {
                    if (game != null)
                    {
                        game.suspend
                        if(chooser.showSaveDialog(null) == FileChooser.Result.Approve)
                        {
                            if (!chooser.selectedFile.getPath.toLowerCase.endsWith(".pgn"))
                                game.getHistory.savePGN(chooser.selectedFile.getPath+".pgn");
                            else
                                game.getHistory.savePGN(chooser.selectedFile.getPath);
                        }
                        game.resume
                    }
                }
                if (source == loadGame)
                {
                    if (game != null) game.suspend
                    if(chooser.showOpenDialog(null) == FileChooser.Result.Approve)
                    {
                        if (chooser.selectedFile.exists)
                        {
                            val history = History.loadPGN(chooser.selectedFile.getPath)
                            if (history != null)
                            {
                                val player = new SimulatedPlayer(history, canvas)
                                game = new Game(canvas, player, player, history.mode, clockSettings)
                                switchToExploreMode
                                currentSimulatedPlayer = player
                                prev.enabled = false ; prev_final.enabled = false
                            }
                        }
                    }
                    else if (game != null) game.resume
                }
                if (source == next)
                {
                    if (currentSimulatedPlayer != null)
                    {
                        if (!currentSimulatedPlayer.playNextMove)
                        {
                            next.enabled = false
                            next_final.enabled = false
                        }
                        prev_final.enabled = true ; prev.enabled = true
                    }
                }
                if (source == next_final)
                {
                    if (currentSimulatedPlayer != null)
                    {
                        currentSimulatedPlayer.playAllMoves
                        next.enabled = false ; next_final.enabled = false
                        prev_final.enabled = true ; prev.enabled = true
                    }
                }
                if (source == prev)
                {
                    if (currentSimulatedPlayer != null && game != null)
                    {
                        game.suspend
                        val i = game.getMoveNumber-2
                        if (i < 0)
                        {
                            prev_final.enabled = false
                            prev.enabled = false
                        }
                        else
                            canvas.ignoreRepaint = true
                        game = new Game(canvas, currentSimulatedPlayer, currentSimulatedPlayer, currentSimulatedPlayer.history.mode, clockSettings)
                        currentSimulatedPlayer.playUntilIndex(i)
                        next.enabled = true ; next_final.enabled = true
                    }
                }
                if (source == prev_final)
                {
                    if (currentSimulatedPlayer != null)
                    {
                        if (game != null) game.suspend
                        game = new Game(canvas, currentSimulatedPlayer, currentSimulatedPlayer, currentSimulatedPlayer.history.mode, clockSettings)
                        next.enabled = true ; next_final.enabled = true
                        prev.enabled = false ; prev_final.enabled = false
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
