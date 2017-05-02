/**
A primitive AI that plays random (legal) moves.
*/
class PrimitiveAI extends SynchPlayer
{
    override def synchPlay (game:Game) : (Int,Int,Int,Int) =
    {
        Thread.sleep(500)
        val moves = game.possibleMoves.toArray
        val r = scala.util.Random
        return moves(r.nextInt(moves.length))
    }
}

/**
An Alpha-Beta AI.
*/
class AlphaBetaAI extends SynchPlayer
{
    val pieceVal = scala.collection.mutable.Map[PieceType.PieceType, Int](
        PieceType.Pawn -> 1,
        PieceType.Rook -> 5,
        PieceType.Knight -> 3,
        PieceType.Bishop -> 3,
        PieceType.Queen -> 9,
        PieceType.King -> 15,
        PieceType.ArchBishop -> 7,
        PieceType.Chancellor -> 7
    )
    def eval (game:Game) : Int =
    {
        var diffPieceVals = 0
        for (i <- 0 to game.dim_x-1)
        {
            for (j <- 0 to game.dim_y-1)
            {
                var p = game.pieceAtPosition(i, j)
                if (p != null)
                    diffPieceVals += i + j + pieceVal(p.pieceType)
            }
        }
        return diffPieceVals
    }

    val minEval = 0
    val maxEval = 1000
    private var maxProf = 6
    private var maxNodes = 1000
    
    def computeMinimax(game:Game,isMyTurn:Boolean,prof:Int,nbNodesEvaluated:Int) : Int =
    {
        if (prof > maxProf || nbNodesEvaluated > maxNodes)
            return eval(game)
        else
        {
            var moves = game.possibleMoves.toArray
            var moveChosenValue = (if (isMyTurn) minEval -1 else maxEval + 1)
            for (moveTest <- moves)
            {
                // TODO : nbNodesEvaluated
                var moveTestValue = computeMinimax(game, !isMyTurn, prof + 1, nbNodesEvaluated)  // TODO : here, game = (simulate moveTest in game)
                if ((isMyTurn && moveTestValue > moveChosenValue) || (!isMyTurn && moveTestValue < moveChosenValue))
                    moveChosenValue = moveTestValue
            }
            return moveChosenValue
        }
    }
    
    def miniMax(game:Game) : (Int,Int,Int,Int) =
    {
        var moves = game.possibleMoves.toArray
        var moveChosen = moves(0)
        var moveChosenValue = minEval - 1
        var moveTestValue = 0
        moves.foreach
        { moveTest =>
            moveTestValue = computeMinimax(game,false,1,1)  // TODO : here, game = (simulate moveTest in game)
            if (moveTestValue > moveChosenValue)
            {
                moveChosen = moveTest
                moveChosenValue = moveTestValue
            }
        }
        return moveChosen
    }

    override def synchPlay (game:Game) : (Int,Int,Int,Int) =
    {
        return miniMax(game)
    }
}
