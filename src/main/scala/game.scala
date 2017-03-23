
import javax.swing.SwingUtilities

/**
Represent a period for the clock.

@param time The time limit, in seconds. If negative, no time limit.
@param moves The duration of the period, in number of rounds. If non-positive, period duration is infinite.
@param increment The additional number of seconds given after each move.
*/
class TimePeriod(val time:Int, val moves:Int, val increment:Int);

/**
Implements a chessboard with all rules (logic of moves, rounds, end of the game, etc).

All methods are thread-safe.

@param canvas The canvas that will display the game.
@param playerWhite The player of the white team.
@param playerBlack The player of the black team.
*/
class Game(private val canvas:Canvas, private var playerWhite:Player, private var playerBlack:Player) extends Board(8,8)
{
    private var round = Round.Black
    private var suspended = false
    private var enPassantPosition : Option[(Int,Int)] = None
    // CurrentConfiguration contains data about current round, possible moves and positions. Used for caching and history.
    private var currentConfiguration:(scala.collection.mutable.Set[PieceStruct],Round.Round,
    scala.collection.mutable.Set[(Int,Int,Int,Int)],scala.collection.mutable.Set[(Int,Int,Int,Int)])
    = (null, Round.Black, null, null)
    private var roundNumber = 0
    private var fmRule = 0
    // ThreefoldCounter is a configuration history
    private var threefoldCounter:scala.collection.mutable.Map
    [(scala.collection.mutable.Set[PieceStruct],Round.Round,scala.collection.mutable.Set[(Int,Int,Int,Int)],scala.collection.mutable.Set[(Int,Int,Int,Int)]),Int]
    = scala.collection.mutable.Map
    [(scala.collection.mutable.Set[PieceStruct],Round.Round,scala.collection.mutable.Set[(Int,Int,Int,Int)],scala.collection.mutable.Set[(Int,Int,Int,Int)]),Int]()
    // Clock
    private var timeControls:TimePeriod = new TimePeriod(4500,0,5) // Should be Array[TimePeriod], in order to support multiple periods. But for now...
    private var clock:scala.collection.mutable.Map[Round.Round,Int] = scala.collection.mutable.Map(Round.White -> 0, Round.Black -> 0)
    private var timer : java.util.Timer = null

    changeRoundAndUpdateConfiguration
    canvas.newGame(this)
    playerWhite.init(this)
    playerBlack.init(this)
    playerWhite.mustPlay
    clock(Round.White) = timeControls.time
    clock(Round.Black) = timeControls.time
    scheduleTimer

    private def scheduleTimer() : Unit =
    {
        val timerTask = new java.util.TimerTask { def run() = {round.synchronized{updateClock}} }
        timer = new java.util.Timer()
        timer.schedule(timerTask, 1000L, 1000L)
    }

    private def changeRoundAndUpdateConfiguration() : Unit =
    {
        // Round & config
        val config:scala.collection.mutable.Set[PieceStruct] = scala.collection.mutable.Set[PieceStruct]()
        for (i<- 0 to dim_x-1)
        {
            for (j<- 0 to dim_y-1)
            {
                val p = pieceAtPosition(i,j)
                if (p != null)
                    config += new PieceStruct(p)
            }
        }
        val p2Moves = calculatePossibleMoves
        round = Round.adv(round)
        val p1Moves = calculatePossibleMoves
        currentConfiguration = (config,round,p1Moves,p2Moves)
    }
    private def calculatePossibleMoves : scala.collection.mutable.Set[(Int,Int,Int,Int)] =
    {
        val set = scala.collection.mutable.Set[(Int,Int,Int,Int)]()
        for (i<-0 to dim_x-1)
        {
            for (j<-0 to dim_y-1)
            {
                if (canMove(i,j))
                {
                    for (k<-0 to dim_x-1)
                    {
                        for (l<-0 to dim_y-1)
                        {
                            if (canMove(i,j,k,l))
                                set += ((i,j,k,l))
                        }
                    }
                }
            }
        }
        return set
    }
    private def gameFinished() : Unit =
    {
        round = Round.Finished
        playerWhite.stop
        playerBlack.stop
        timer.cancel
    }
    private def updateClock() : Unit =
    {
        if (clock(round) > 0)
            clock(round) -= 1
        // Check if time is over.
        if (clock(round) == 0)
        {
            canvas.setMessage ("Time elapsed ! " + Round.adv(round) + " wins !")
            gameFinished
        }
        canvas.repaint
    }
    /**
    Get the current clock of the player.
    */
    def getClock (t:Round.Round) = { round.synchronized{ clock(t) } }

    /**
    Suspends the game and every running thread. Game can be resumed later.
    */
    def suspend =
    {
        round.synchronized
        {
            if (!suspended && round != Round.Finished)
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
        round.synchronized
        {
            if (suspended && round != Round.Finished)
            {
                suspended = false
                playerWhite.init(this)
                playerBlack.init(this)
                if (round == Round.White)
                    playerWhite.mustPlay
                if (round == Round.Black)
                    playerBlack.mustPlay
                scheduleTimer
            }
        }
    }

    /**
    Change the current white player. Only works if the game is suspended.
    */
    def setWhitePlayer(p:Player) : Unit = { round.synchronized { if (suspended) playerWhite = p } }
    /**
    Change the current black player. Only works if the game is suspended.
    */
    def setBlackPlayer(p:Player) : Unit = { round.synchronized { if (suspended) playerBlack = p } }

    /**
    Returns the current round.
    */
    def getRound = { round.synchronized{ round } }

    /**
    Returns the current round number.
    */
    def getRoundNumber = { round.synchronized{ roundNumber/2 + 1 } }

    /**
    Returns the current round counter in the context of the 50-move rule.
    */
    def getFiftyMoveRuleCounter = { round.synchronized{ fmRule/2 } }

    /**
    Get threefold repetion counter.
    */
    def getThreefoldRepetitionCounter() : Int =
    { 
        round.synchronized
        {
            return (threefoldCounter getOrElse (currentConfiguration, 0)) + 1
        }
    }

    /**
    Indicates whether the piece at the given position can be moved.
    */
    def canMove(x:Int,y:Int):Boolean =
    {
        round.synchronized
        {
            val piece = pieceAtPosition(x,y)
            if (piece != null)
                if (piece.team == round)
                    return true
            return false
        }
    }

    /**
    Return None if the move is not a valid 'en passant' move. Otherwise, return the position of the eaten piece.
    */
    def enPassantMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Option[(Int,Int)] =
    {
        round.synchronized
        {
            enPassantPosition match
            {
                case None => {return None}
                case Some ((fX,fY)) => { if (fX != toX || fY != toY) return None }
            }
            if (!canMove(fromX,fromY))
                return None
            val p = pieceAtPosition(fromX,fromY)
            if (p.pieceType != PieceType.Pawn)
                return None
            if (math.abs(fromX-toX) != 1)
                return None
            if (round == Round.White && fromY-toY != 1)
                return None
            if (round == Round.Black && toY-fromY != 1)
                return None

            // Not check
            val cboard = new Board(this)
            cboard.move(fromX,fromY,toX,toY)
            cboard.remove(toX,fromY)
            if (cboard.getKing(round).isCheck)
                return None
            return Some (toX,fromY)
        }
    }
    /**
    Return None if the move is not a valid castling move. Otherwise, return the move implied for the rook.
    */
    def castlingMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Option[(Int,Int,Int,Int)] =
    {
        round.synchronized
        {
            // 1. King and rook have not moved previously in this game
            if (!canMove(fromX,fromY))
                return None
            val p = pieceAtPosition(fromX,fromY)
            if (p.pieceType != PieceType.King || p.hasMoved)
                return None
            if (fromY != toY || math.abs(toX-fromX) != 2)
                return None
            val dir = Direction.directionApplied(fromX,fromY,toX,toY)

            var rookX = fromX
            var rookY = fromY
            if (dir == Direction.Left)
                rookX = 0
            if (dir == Direction.Right)
                rookX = dim_x-1
            if (!canMove(rookX,rookY))
                return None
            val rook = pieceAtPosition(rookX,rookY)
            if (rook.pieceType != PieceType.Rook || rook.hasMoved)
                return None

            // 2. No piece between king and rook
            if (!p.noObstacleTo(rookX, rookY))
                return None

            // 3. Not check at the first/intermediate/last position
            val cboard = new Board(this)
            if (cboard.getKing(round).isCheck)
                return None
            val (intermediateX, intermediateY) = Direction.applyDirection(fromX, fromY, dir)
            cboard.move(fromX,fromY,intermediateX,intermediateY)
            if (cboard.getKing(round).isCheck)
                return None
            cboard.move(intermediateX,intermediateY,toX,toY)
            if (cboard.getKing(round).isCheck)
                return None

            return Some(rookX,rookY,intermediateX,intermediateY)
        }
    }

    /**
    Indicates whether the piece at the first position can be moved to the second position.
    */
    def canMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Boolean =
    {
        round.synchronized
        {
            if (!canMove(fromX,fromY))
                return false

            // Special move : Castling, 'En Passant'
            if (castlingMove(fromX,fromY,toX,toY) != None || enPassantMove(fromX,fromY,toX,toY) != None)
                return true
                
            // Regular move
            val p = pieceAtPosition(fromX,fromY)
            if (!p.canMove(toX, toY))
                return false

            // Checking if the king become check
            val cboard = new Board(this)
            cboard.move(fromX,fromY,toX,toY)
            if (cboard.getKing(round).isCheck)
                return false
            return true
        }
    }
    /**
    Returns a list of all the possible moves.

    Using this function is FASTER than calling canMove multiple times : it does not recalculate every move.
    */
    def possibleMoves : scala.collection.mutable.Set[(Int,Int,Int,Int)] =
    {
        round.synchronized
        {
            val (_,_,p1moves,_) = currentConfiguration
            return p1moves
        }
    }
    /**
    Indicates whether a draw can be requested by the current player.
    */
    def canRequestDraw():Boolean =
    {
        round.synchronized
        {
            if (round == Round.Finished)
                return false
            if (getFiftyMoveRuleCounter >= 50)
                return true
            if (getThreefoldRepetitionCounter >= 3)
                return true
            return false
        }
    }
    /**
    Request draw. The request must be legit (50-move rule...)
    */
    def requestDraw():Unit =
    {
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _requestDraw }
        });
    }
    private def _requestDraw():Unit =
    {
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return

            if (!canRequestDraw)
                return
            
            canvas.setMessage ("Draw !")
            gameFinished
            canvas.repaint
        }
    }
    /**
    Resignation of the player. The opponent will win the game.
    */
    def resign():Unit =
    {
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _resign }
        });
    }
    private def _resign():Unit =
    {
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return
            
            canvas.setMessage ("Resignation ! " + Round.adv(round) + " wins !")
            gameFinished
            canvas.repaint
        }
    }
    /**
    Plays the given move. The move must be legal.

    The last optional parameter is the type of piece wanted in the case of a promotion (default is queen).
    */
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int, promotionType:PieceType.PieceType, drawAfterMove:Boolean = false):Unit =
    {
        // We must be on the main thread before making some modifications.
        // if (!SwingUtilities.isEventDispatchThread())
        // Even if we are already on the main thread, we must reinvoke this function later (avoid interlaced turns).
        SwingUtilities.invokeLater(new Runnable() {
            override def run  : Unit = { _move(fromX,fromY,toX,toY,promotionType, drawAfterMove) }
        });
    }
    private def _move(fromX:Int,fromY:Int,toX:Int,toY:Int,promotionType:PieceType.PieceType, drawAfterMove:Boolean):Unit =
    {
        round.synchronized
        {
            if (suspended || round == Round.Finished)
                return
            
            if (!canMove(fromX,fromY,toX,toY))
                return
            
            // Updating counters
            fmRule += 1
            threefoldCounter(currentConfiguration) = (threefoldCounter getOrElse (currentConfiguration, 0)) + 1
            if (pieceAtPosition(fromX,fromY).pieceType == PieceType.Pawn) // Detect if the piece moved is a pawn
                fmRule = 0
            else if (pieceAtPosition(toX,toY) != null) // Detect if a piece is eaten
                fmRule = 0
            else if (enPassantMove(fromX,fromY,toX,toY) != None) // Detect if a piece is eaten 'en passant'
                fmRule = 0
            if (fmRule == 0) // We clear the threefoldCounter if possible
                threefoldCounter.clear
            roundNumber += 1
            clock(round) += timeControls.increment

            // Do the move !!!
            castlingMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY,tX,tY)) => {super.move (fX,fY,tX,tY)}
            }
            enPassantMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY)) => {super.remove (fX,fY)}
            }
            enPassantPosition = None
            if (pieceAtPosition(fromX,fromY).pieceType == PieceType.Pawn && math.abs(toY-fromY) == 2)
                enPassantPosition = Some (fromX+(toX-fromX)/2, fromY+(toY-fromY)/2)
            super.move (fromX,fromY,toX,toY)
            if ((toY == 0 || toY == dim_y-1) && pieceAtPosition(toX,toY).pieceType == PieceType.Pawn)
            {
                if (promotionType == PieceType.Knight)
                    super.add(new Knight(round, this, toX, toY))
                else if (promotionType == PieceType.Bishop)
                    super.add(new Bishop(round, this, toX, toY))
                else if (promotionType == PieceType.Rook)
                    super.add(new Rook(round, this, toX, toY))
                else
                    super.add(new Queen(round, this, toX, toY))
            }
            changeRoundAndUpdateConfiguration

            // Reinit clock if new period
            if (timeControls.moves > 0)
                if ((getRoundNumber - 1) % timeControls.moves == 0)
                    clock(round) = timeControls.time

            // Preparing next round
            if (drawAfterMove && canRequestDraw)
            {
                // Draw requested
                requestDraw
            }
            else
            {
                // Check/Checkmate/Stalemate detection
                val check = getKing(round).isCheck
                val noMove = possibleMoves.isEmpty

                if (check && noMove)
                    canvas.setMessage ("Checkmate ! " + Round.adv(round).toString + " wins !")
                else if (check)
                    canvas.setMessage ("Check !")
                else if (noMove)
                    canvas.setMessage ("Stalemate !")
                else
                    canvas.clearMessage

                if (noMove)
                    gameFinished

                canvas.repaint
                if (round == Round.White)
                    playerWhite.mustPlay
                if (round == Round.Black)
                    playerBlack.mustPlay
            }
        }
    }

    override def remove(fromX:Int,fromY:Int) : Unit = { }
    override def add(p:Piece):Unit = { }
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit = { move(fromX,fromY,toX,toY,PieceType.Unknown) }

}
