import swing._
import swing.event._
import java.awt.Dimension

object MyApp extends SimpleSwingApplication {
	def top = new MainFrame {

		title = "Checks"

        val canvas = new Canvas(1000,750)
        var game:Game = null

        // Buttons
        val human = new Button("Human Fight !")
        val human_AI = new Button("Human vs AI !")
        val AI = new Button("AI Fight !")
        val settings = new Button("Settings")
        val buttons = new FlowPanel
        {
            contents += human
            contents += human_AI
            contents += AI
            contents += settings
        }

        // Settings
        val settingsPanel = new FlowPanel // TODO : Move to settings.scala
        {
            contents += new Button("Test")
        }

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
                    human.visible = false
                    human_AI.visible = false
                    AI.visible = false
                }
                else
                {
                    settings.text = "Settings"
                    human.visible = true
                    human_AI.visible = true
                    AI.visible = true
                }
            }
            def settingsDisplayed() : Boolean = { settingsPanel.visible }
        }
        contents = content

        listenTo(human, human_AI, AI, settings, this)
        reactions += {
            case ButtonClicked (source) => {
                    if (source == human)
                    {
                        if (game != null) game.suspend
                        game = new Game(canvas, canvas, canvas)
                    }
                    if (source == human_AI)
                    {
                        if (game != null) game.suspend
                        game = new Game(canvas, canvas, new PrimitiveAI)
                    }
                    if (source == AI)
                    {
                        if (game != null) game.suspend
                        game = new Game(canvas, new PrimitiveAI, new PrimitiveAI)
                    }
                    if (source == settings)
                    {
                        if (game != null && !content.settingsDisplayed) game.suspend
                        content.switch
                        if (game != null && !content.settingsDisplayed) game.resume
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
