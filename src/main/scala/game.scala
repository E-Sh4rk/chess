
import javax.swing.SwingUtilities

/**
Represents a period for the clock.

@param time The time limit, in seconds. If negative, no time limit.
@param moves The duration of the period, in number of rounds. If non-positive, period duration is infinite.
@param increment The additional number of seconds given after each move.
*/
class TimePeriod(val time:Int, val moves:Int, val increment:Int);

/**
Implements a chessboard with all rules (logic of moves, rounds, clock, etc).

All methods are thread-safe.

@param canvas The canvas that will display the game.
@param playerWhite The player of the white team.
@param playerBlack The player of the black team.
@param gameMode The game mode.
@param clockSettings The settings for the clock.
*/
class Game(private val canvas:Canvas, private var playerWhite:Player, private var playerBlack:Player,
           private val gameMode:GameMode.GameMode, private val clockSettings:TimePeriod) extends Rules(gameMode)
{
    private var suspended = false
    // Clock
    // clockSettings should be Array[TimePeriod] in order to support multiple periods. But for now...
    private var clock:scala.collection.mutable.Map[Round.Round,Int] = scala.collection.mutable.Map(Round.White -> 0, Round.Black -> 0, Round.Finished -> 0)
    private var timer : java.util.Timer = null

    clock(Round.White) = clockSettings.time
    clock(Round.Black) = clockSettings.time

    canvas.newGame(this)
    playerWhite.init(this)
    playerBlack.init(this)
    callPlayers(None)
    scheduleTimer

    private def callPlayers(lastMove:Option[(Int,Int,Int,Int)]):Unit =
    {
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit =
            {
                this.synchronized
                {
                    if (suspended || getRound == Round.Finished)
                        return
                    if (getRound == Round.White)
                        playerWhite.mustPlay(lastMove)
                    if (getRound == Round.Black)
                        playerBlack.mustPlay(lastMove)
                }
            }
        });
    }
    private def refreshCanvas():Unit =
    {
        if (!SwingUtilities.isEventDispatchThread())
        {
            SwingUtilities.invokeLater(new Runnable() {
                override def run  : Unit = { refreshCanvas }
            });
        }
        else
            this.synchronized { canvas.repaint }
    }
    private def scheduleTimer() : Unit =
    {
        val timerTask = new java.util.TimerTask { def run() = {this.synchronized{updateClock}} }
        timer = new java.util.Timer()
        timer.schedule(timerTask, 1000L, 1000L)
    }

    private def gameFinished() : Unit =
    {
        playerWhite.stop
        playerBlack.stop
        timer.cancel
    }
    private def updateClock() : Unit =
    {
        val old = clock(getRound)
        if (clock(getRound) > 0)
            clock(getRound) -= 1
        if (clock(getRound) == 0)
        {
            if (endForAnotherReason("Time elapsed ! " + Round.adv(getRound) + " wins !"))
                gameFinished
        }
        if (old != clock(getRound))
            refreshCanvas
    }
    /**
    Gets the current clock of the player.
    */
    def getClock (t:Round.Round) = { this.synchronized{ clock(t) } }

    /**
    Suspends the game and every running thread. Game can be resumed later.
    */
    def suspend =
    {
        this.synchronized
        {
            if (!suspended && getRound != Round.Finished)
            {
                suspended = true
                playerWhite.stop
                playerBlack.stop
                timer.cancel
            }
        }
    }
    /**
    Resumes a suspended game.
    */
    def resume =
    {
        this.synchronized
        {
            if (suspended && getRound != Round.Finished)
            {
                suspended = false
                playerWhite.init(this)
                playerBlack.init(this)
                callPlayers(None)
                scheduleTimer
            }
        }
    }

    /**
    Changes the current white player. Only works if the game is suspended.
    */
    def setWhitePlayer(p:Player) : Unit = { this.synchronized { if (suspended) playerWhite = p } }
    /**
    Changes the current black player. Only works if the game is suspended.
    */
    def setBlackPlayer(p:Player) : Unit = { this.synchronized { if (suspended) playerBlack = p } }

    /**
    Draw requested. The request must be legit (50-move rule...)
    */
    override def requestDraw():Boolean =
    {
        this.synchronized
        {
            if (suspended)
                return false
            if (super.requestDraw)
            {
                gameFinished
                refreshCanvas
                return true
            }
            return false
        }
    }

    /**
    Resignation of the player. The opponent will win the game.
    */
    override def resign():Boolean =
    {
        this.synchronized
        {
            if (suspended)
                return false
            if (super.resign)
            {
                gameFinished
                refreshCanvas
                return true
            }
            return false
        }
    }

    /**
    Plays the given move. The move must be legal.

    The last optional parameter is the type of piece wanted in the case of a promotion (default is queen).
    */
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int, promotionType:PieceType.PieceType, drawAfterMove:Boolean = false):Boolean =
    {
        this.synchronized
        {
            if (suspended)
                return false
            if (super.move(fromX,fromY,toX,toY,promotionType, drawAfterMove))
            {
                clock(getRound) += clockSettings.increment
                // Reinitializes clock if new period
                if (clockSettings.moves > 0)
                    if ((getRoundNumber - 1) % clockSettings.moves == 0)
                        clock(getRound) = clockSettings.time

                if (getRound == Round.Finished)
                    gameFinished

                refreshCanvas
                callPlayers(Some((fromX,fromY,toX,toY)))
                return true
            }
            return false
        }
    }

}
