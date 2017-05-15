
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
           private val gameMode:GameMode.GameMode, private val clockSettings:Array[TimePeriod]) extends Rules(gameMode)
{
    private var suspended = false
    // Clock
    private var clock:scala.collection.mutable.Map[Round.Round,Int] = scala.collection.mutable.Map(Round.White -> 0, Round.Black -> 0, Round.Finished -> 0)
    private var timer : java.util.Timer = null
    private var roundCycle : Int = computeRoundCycle

    clock(Round.White) = getNewTime(-1)
    clock(Round.Black) = getNewTime(-1)

    canvas.newGame(this)
    initPlayers
    callPlayers(null)
    scheduleTimer

    private def computeRoundCycle() : Int =
    {
        var n:Int = 0
        for (i <- 0 to clockSettings.length - 1)
        {
            if (clockSettings(i).moves <= 0)
                return 0
            n += clockSettings(i).moves
        }
        return n
    }
    private def getNewTime(current:Int) : Int =
    {
        var round = getRoundNumber-1
        if (roundCycle > 0)
            round = round % roundCycle
        for (i <- 0 to clockSettings.length -1)
        {
            // We are not in this period yet
            if (round < 0)
                return current + clockSettings(i-1).increment
            // We have just entered this period
            if (round == 0)
                return clockSettings(i).time
            // Testing next period, if any
            if (clockSettings(i).moves <= 0 || i >= clockSettings.length-1)
                return current + clockSettings(i).increment
            round -= clockSettings(i).moves
        }
        return -1
    }

    private def initPlayers():Unit =
    {
        playerWhite.init(this)
        if (playerBlack ne playerWhite)
            playerBlack.init(this)
    }
    private def stopPlayers():Unit =
    {
        playerWhite.stop
        if (playerBlack ne playerWhite)
            playerBlack.stop
    }
    private def callPlayers(lastMove:Move):Unit =
    {
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit =
            {
                this.synchronized
                {
                    if (suspended || getRound == Round.Finished)
                        return

                    var lm = lastMove
                    if (playerWhite eq playerBlack)
                        lm = null

                    if (getRound == Round.White)
                        playerWhite.mustPlay(lm)
                    if (getRound == Round.Black)
                        playerBlack.mustPlay(lm)
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
        stopPlayers
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
                stopPlayers
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
                initPlayers
                callPlayers(null)
                scheduleTimer
            }
        }
    }
    /**
    Indicates whether a game is suspended or not.
    */
    def isSuspended : Boolean = { this.synchronized { return suspended } }

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
                clock(getRound) = getNewTime(clock(getRound))

                if (getRound == Round.Finished)
                    gameFinished

                callPlayers(getHistory.moves.last)
                refreshCanvas
                return true
            }
            return false
        }
    }

}
