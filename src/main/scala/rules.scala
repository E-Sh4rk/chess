
import javax.swing.SwingUtilities

/**
Implements a chessboard with all rules about movements and rounds.

All methods are thread-safe.

@param _r The instance of Rule to copy.
@param _gm The game mode.
*/
class Rules(private val _r:Rules, private val _gm:GameMode.GameMode) extends Board(_r, _gm)
{
    private var message:String = null
    private var round = Round.Black
    private var opponentRequestedDraw = false
    private var enPassantPosition : Option[(Int,Int)] = None
    private var history:History = new History
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

    if (_r == null)
    {
        history.mode = _gm
        history.dim_x = dim_x
        history.dim_y = dim_y
        changeRoundAndUpdateConfiguration
    }
    else
    {
        history.mode = _r.history.mode
        history.dim_x = _r.history.dim_x
        history.dim_y = _r.history.dim_y

        round = _r.round
        opponentRequestedDraw = _r.opponentRequestedDraw
        enPassantPosition = _r.enPassantPosition

        // NOTE : History is not copied (no need for that)
        currentConfiguration = copyConfiguration(_r.currentConfiguration)
        for (_k <- _r.threefoldCounter.keys)
            threefoldCounter(copyConfiguration(_k)) = _r.threefoldCounter(_k)

        roundNumber = _r.roundNumber
        fmRule = _r.fmRule

    }
    def this (_r:Rules) = { this (_r, GameMode.Vanilla) }
    def this (_gm:GameMode.GameMode) = { this (null, _gm) }
    def this () = { this (GameMode.Vanilla) }

    private def copyConfiguration(_conf:(scala.collection.mutable.Set[PieceStruct],Round.Round,
    scala.collection.mutable.Set[(Int,Int,Int,Int)],scala.collection.mutable.Set[(Int,Int,Int,Int)]))
    : (scala.collection.mutable.Set[PieceStruct],Round.Round,scala.collection.mutable.Set[(Int,Int,Int,Int)],scala.collection.mutable.Set[(Int,Int,Int,Int)]) =
    {
        val (_pieces,_round,_moves1,_moves2) = _conf
        var pieces = scala.collection.mutable.Set[PieceStruct]()
        for (_p <- _pieces)
            pieces += new PieceStruct(_p.pieceType, _p.team, _p.pos)
        var moves1 = scala.collection.mutable.Set[(Int,Int,Int,Int)]()
        for (_m <- _moves1)
            moves1 += _m
        var moves2 = scala.collection.mutable.Set[(Int,Int,Int,Int)]()
        for (_m <- _moves2)
            moves2 += _m
        return  (pieces,_round,moves1,moves2)
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
        val p2Moves = computePossibleMoves
        round = Round.adv(round)
        val p1Moves = computePossibleMoves
        currentConfiguration = (config,round,p1Moves,p2Moves)
    }

    private def computePossibleMoves : scala.collection.mutable.Set[(Int,Int,Int,Int)] =
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

    /**
    Returns the history of the current game.
    */
    def getHistory() = { this.synchronized{ history } }

    /**
    Returns informations about the game. If there is nothing special, return null.
    */
    def getMessage = { this.synchronized{ message } }

    /**
    Returns the current round.
    */
    def getRound = { this.synchronized{ round } }

    /**
    Returns the current round number.
    */
    def getRoundNumber = { this.synchronized{ roundNumber/2 + 1 } }

    /**
    Returns the current move number.
    */
    def getMoveNumber = { this.synchronized{ roundNumber } }

    /**
    Returns the current round counter in the context of the 50-move rule.
    */
    def getFiftyMoveRuleCounter = { this.synchronized{ fmRule/2 } }

    /**
    Gets threefold repetion counter.
    */
    def getThreefoldRepetitionCounter() : Int =
    { 
        this.synchronized
        {
            return (threefoldCounter getOrElse (currentConfiguration, 0)) + 1
        }
    }

    /**
    Indicates whether the piece at the given position can be moved.
    */
    def canMove(x:Int,y:Int):Boolean =
    {
        this.synchronized
        {
            val piece = pieceAtPosition(x,y)
            if (piece != null)
                if (piece.team == round)
                    return true
            return false
        }
    }

    /**
    Returns None if the move is not a valid 'en passant' move. Otherwise returns the position of the eaten piece.
    */
    def enPassantMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Option[(Int,Int)] =
    {
        this.synchronized
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
    Returns None if the move is not a valid castling move. Otherwise returns the move implied for the rook.
    */
    def castlingMove(fromX:Int,fromY:Int,toX:Int,toY:Int):Option[(Int,Int,Int,Int)] =
    {
        this.synchronized
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
        this.synchronized
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

    Using this function is FASTER than calling canMove multiple times : it does not recompute every move.
    */
    def possibleMoves : scala.collection.mutable.Set[(Int,Int,Int,Int)] =
    {
        this.synchronized
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
        this.synchronized
        {
            if (round == Round.Finished)
                return false
            if (opponentRequestedDraw)
                return true
            if (getFiftyMoveRuleCounter >= 50)
                return true
            if (getThreefoldRepetitionCounter >= 3)
                return true
            return false
        }
    }
    /**
    Draw requested. The request must be legit (50-move rule...)
    */
    def requestDraw():Boolean =
    {
        this.synchronized
        {
            if (round == Round.Finished)
                return false
            if (!canRequestDraw)
                return false
            message = "Draw !"
            round = Round.Finished
            return true
        }
    }
    /**
    Resignation of the player. The opponent will win the game.
    */
    def resign():Boolean =
    {
        this.synchronized
        {
            if (round == Round.Finished)
                return false
            message = "Resignation ! " + Round.adv(round) + " wins !"
            round = Round.Finished
            return true
        }
    }

    /**
    Terminate the game for a reason not implemented in this class (clock...). Give the reason as parameter.
    */
    protected def endForAnotherReason(msg:String) : Boolean =
    {
        this.synchronized
        {
            if (round == Round.Finished)
                return false
            message = msg
            round = Round.Finished
            return true
        }
    }

    /**
    Plays the given move. The move must be legal.

    The last optional parameter is the type of piece wanted in the case of a promotion (default is queen).
    */
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int,promotionType:PieceType.PieceType, drawAfterMove:Boolean = false):Boolean =
    {
        this.synchronized
        {
            if (round == Round.Finished)
                return false
            
            if (!canMove(fromX,fromY,toX,toY))
                return false

            // Vars for history
            var h_type:PieceType.PieceType = pieceAtPosition(fromX,fromY).pieceType
            var h_catch:Boolean = false
            var h_castle:CastleType.CastleType = CastleType.NoCastle
            var h_promotion:PieceType.PieceType = PieceType.Unknown
            var h_event:GameEvent.GameEvent = GameEvent.NoEvent
            
            // Updating counters
            fmRule += 1
            threefoldCounter(currentConfiguration) = (threefoldCounter getOrElse (currentConfiguration, 0)) + 1
            if (pieceAtPosition(fromX,fromY).pieceType == PieceType.Pawn) // Detects if the piece moved is a pawn
                fmRule = 0
            if (pieceAtPosition(toX,toY) != null) // Detects if a piece is eaten
                { h_catch = true ; fmRule = 0 }
            else if (enPassantMove(fromX,fromY,toX,toY) != None) // Detects if a piece is eaten 'en passant'
                { h_catch = true ; fmRule = 0 }
            if (fmRule == 0) // Clears the threefoldCounter if possible
                threefoldCounter.clear
            roundNumber += 1

            // Do the move !!!
            castlingMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY,tX,tY)) =>
                {
                    h_castle = if ( 2 * fromX / dim_x == 2 * toX / dim_x ) CastleType.Kingside else CastleType.Queenside
                    super.move (fX,fY,tX,tY)
                }
            }
            enPassantMove(fromX,fromY,toX,toY) match
            {
                case None => {}
                case Some ((fX,fY)) => {super.remove (fX,fY)}
            }
            enPassantPosition = None
            if (pieceAtPosition(fromX,fromY).pieceType == PieceType.Pawn && math.abs(toY-fromY) == 2)
                enPassantPosition = Some ((toX+fromX)/2, (toY+fromY)/2)
            super.move (fromX,fromY,toX,toY)
            if ((toY == 0 || toY == dim_y-1) && pieceAtPosition(toX,toY).pieceType == PieceType.Pawn)
            {
                h_promotion = promotionType
                if (promotionType == PieceType.Knight)
                    super.add(new Knight(round, this, toX, toY))
                else if (promotionType == PieceType.Bishop)
                    super.add(new Bishop(round, this, toX, toY))
                else if (promotionType == PieceType.Rook)
                    super.add(new Rook(round, this, toX, toY))
                else
                {
                    h_promotion = PieceType.Queen
                    super.add(new Queen(round, this, toX, toY))
                }
            }
            changeRoundAndUpdateConfiguration
            opponentRequestedDraw = false

            // Check/Checkmate/Stalemate detection
            val check = getKing(round).isCheck
            val noMove = possibleMoves.isEmpty

            if (check && noMove)
                h_event = GameEvent.Checkmate
            else if (check)
                h_event = GameEvent.Check

            // Adds move to history !
            history.moves.append(new Move(h_type, fromX, fromY, toX, toY, h_catch, h_castle, h_promotion, h_event))

            // Preparing next round
            if (drawAfterMove && canRequestDraw)
            {
                message = "Draw !"
                round = Round.Finished
            }
            else
            {
                if (drawAfterMove)
                    opponentRequestedDraw = true

                if (check && noMove)
                    message = "Checkmate ! " + Round.adv(round).toString + " wins !"
                else if (check)
                    message = "Check !"
                else if (noMove)
                    message = "Stalemate !"
                else
                    message = null  

                if (noMove)
                    round = Round.Finished
            }
                        
            return true
        }
    }

    override def remove(fromX:Int,fromY:Int) : Unit = { }
    override def add(p:Piece):Unit = { }
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit = { move(fromX,fromY,toX,toY,PieceType.Unknown) }

}
