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
    def eval (rules:Rules) : Int =
    {
        var diffPieceVals = 0
        for (i <- 0 to rules.dim_x-1)
        {
            for (j <- 0 to rules.dim_y-1)
            {
                var p = rules.pieceAtPosition(i, j)
                if (p != null)
                    diffPieceVals += i + j + pieceVal(p.pieceType)
            }
        }
        return diffPieceVals
    }

    val minEval = 0
    val maxEval = 1000
    private var maxProf = 4

    def computeAlphaBeta(rules:Rules,isMyTurn:Boolean,alpha:Int,beta:Int,prof:Int) : Int =
    {
        if (alpha > beta)
            return (if (isMyTurn) maxEval else minEval)
        else if (prof > maxProf)
            return eval(rules)
        else
        {
            var moves = rules.possibleMoves
            var bestVal = (if (isMyTurn) minEval else maxEval)
            var alphaCurrent = alpha
            var betaCurrent = beta
            for ((fromX, fromY, toX, toY) <- moves)
            {
                var rulesTest = new Rules(rules, rules.getHistory.mode)
                rulesTest.move(fromX, fromY, toX, toY)
                var valTest = computeAlphaBeta(rules, !isMyTurn, alphaCurrent, betaCurrent, prof + 1)
                if (isMyTurn)
                {
                    if (valTest > bestVal)
                    {
                        bestVal = valTest
                        //beta pruning
                        if (bestVal >= beta)
                            return bestVal
                    }
                    if (bestVal > alphaCurrent)
                        alphaCurrent = bestVal
                }
                else
                {
                    if (valTest < bestVal)
                    {
                        bestVal = valTest
                        //alpha pruning
                        if (bestVal >= beta)
                            return bestVal
                    }
                    if (bestVal < betaCurrent)
                        betaCurrent = bestVal
                }
            }
            return bestVal
        }
    }
    
    def alphaBeta(rules:Rules) : (Int,Int,Int,Int) =
    {
        val moves = rules.possibleMoves
        var moveChosen = moves.head
        var moveChosenValue = minEval
        var moveTestValue = 0
        for (moveTest <- moves)
        {
            val (fromX, fromY, toX, toY) = moveTest
            var rulesTest = new Rules(rules, rules.getHistory.mode)
            rulesTest.move(fromX, fromY, toX, toY)
            moveTestValue = computeAlphaBeta(rulesTest, false, moveChosenValue, maxEval, 1)
            if (moveTestValue >= moveChosenValue)
            {
                moveChosen = moveTest
                moveChosenValue = moveTestValue
            }
        }
        return moveChosen
    }

    override def synchPlay (rules:Game) : (Int,Int,Int,Int) =
    {
        return alphaBeta(rules)
    }
}
