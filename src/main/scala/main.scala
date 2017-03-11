import swing._
import swing.event._
import java.awt.Dimension

object MyApp extends SimpleSwingApplication {
	def top = new MainFrame {

		title = "Checks"

        val canvas = new Canvas(750,750)
        var game:Game = null

        val human = new Button("Human Fight !")
        val human_AI = new Button("Human vs AI !")
        val AI = new Button("AI Fight !")
        val buttons = new FlowPanel
        {
            contents += human
            contents += human_AI
            contents += AI
        }

        val content = new BorderPanel
        {
            add (canvas, BorderPanel.Position.North)
            add (buttons, BorderPanel.Position.South)
        }
        contents = content

        listenTo(human, human_AI, AI, this)
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
