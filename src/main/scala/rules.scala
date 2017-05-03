
import javax.swing.SwingUtilities

/**
Implements a chessboard with all rules about movements and rounds.

All methods are thread-safe.

@param gameMode The game mode.
*/
class Rules(private val _r:Rules, private val gameMode:GameMode.GameMode) extends Board(gameMode) // TODO : Use this class in Game.
{
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

    history.mode = gameMode
    history.dim_x = dim_x
    history.dim_y = dim_y

    if (_r == null)
        changeRoundAndUpdateConfiguration
    else
    {
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
    def this (gameMode:GameMode.GameMode) = { this (null, gameMode) }
    def this () = { this (GameMode.Vanilla) }

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
    def getHistory() = { round.synchronized{ history } }
    /**
    Returns the current round.
    */
    def getRound = { round.synchronized{ round } }

    /**
    Returns the current round number.
    */
    def getRoundNumber = { round.synchronized{ roundNumber/2 + 1 } }

    /**
    Returns the current move number.
    */
    def getMoveNumber = { round.synchronized{ roundNumber } }

    /**
    Returns the current round counter in the context of the 50-move rule.
    */
    def getFiftyMoveRuleCounter = { round.synchronized{ fmRule/2 } }

    /**
    Gets threefold repetion counter.
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
    Returns None if the move is not a valid 'en passant' move. Otherwise returns the position of the eaten piece.
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
    Returns None if the move is not a valid castling move. Otherwise returns the move implied for the rook.
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

    Using this function is FASTER than calling canMove multiple times : it does not recompute every move.
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
    def requestDraw():Unit =
    {
        round.synchronized
        {
            if (round == Round.Finished)
                return
            if (!canRequestDraw)
                return
            round = Round.Finished
        }
    }
    /**
    Resignation of the player. The opponent will win the game.
    */
    def resign():Unit =
    {
        round.synchronized
        {
            if (round == Round.Finished)
                return
            round = Round.Finished
        }
    }

    /**
    Plays the given move. The move must be legal.

    The last optional parameter is the type of piece wanted in the case of a promotion (default is queen).
    */
    def move(fromX:Int,fromY:Int,toX:Int,toY:Int,promotionType:PieceType.PieceType, drawAfterMove:Boolean = false):Unit =
    {
        round.synchronized
        {
            if (round == Round.Finished)
                return
            
            if (!canMove(fromX,fromY,toX,toY))
                return

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
            else if (pieceAtPosition(toX,toY) != null) // Detects if a piece is eaten
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
                requestDraw
            else
            {
                if (drawAfterMove)
                    opponentRequestedDraw = true
                if (noMove)
                    round = Round.Finished
            }
        }
    }

    override def remove(fromX:Int,fromY:Int) : Unit = { }
    override def add(p:Piece):Unit = { }
    override def move(fromX:Int,fromY:Int,toX:Int,toY:Int) : Unit = { move(fromX,fromY,toX,toY,PieceType.Unknown) }

}
