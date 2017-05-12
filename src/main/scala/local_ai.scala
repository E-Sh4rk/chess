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

abstract class EvalFunc()
{
    def eval(rules:Rules) : Int
}


/**
A dumb evaluation function to check the correctness of the AIs
*/
class EvalToy() extends EvalFunc()
{
    val pieceVal = scala.collection.mutable.Map[PieceType.PieceType, Int](
        PieceType.Pawn -> 1,
        PieceType.Rook -> 5,
        PieceType.Knight -> 3,
        PieceType.Bishop -> 3,
        PieceType.Queen -> 9,
        PieceType.King -> 15,
        PieceType.ArchBishop -> 7,
        PieceType.Chancellor -> 7,
        PieceType.Unknown -> 0
    )
    def eval(rules:Rules) : Int =
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
}

class EvalStd() extends EvalFunc()
{
    val dim_x = 8
    val dim_y = 8
    private var isEndGame = false
    
    val pieceVal = scala.collection.mutable.Map[PieceType.PieceType, Int](
        PieceType.Pawn -> 1,
        PieceType.Knight -> 3,
        PieceType.Bishop -> 3,
        PieceType.Rook -> 5,
        PieceType.Queen -> 9,
        PieceType.King -> 20,
        PieceType.ArchBishop -> 7,
        PieceType.Chancellor -> 7,
        PieceType.Unknown -> 0
    )
    val squareVal = scala.collection.mutable.Map[PieceType.PieceType, Array[Array[Int]]] (
        PieceType.Pawn -> squareValuesPawn,
        PieceType.Knight -> squareValuesKnight,
        PieceType.Bishop -> squareValuesBishop,
        PieceType.Rook -> squareValuesRook,
        PieceType.Queen -> squareValuesQueen,
        PieceType.King -> (if (isEndGame) squareValuesKingEnd else squareValuesKingMid), //TODO : évaluation non dynamique
        PieceType.ArchBishop -> squareValuesDefault,
        PieceType.Chancellor -> squareValuesDefault,
        PieceType.Unknown -> squareValuesDefault
    )
    /**
    Special thanks to : https://chessprogramming.wikispaces.com/Simplified+evaluation+function
    for inspiration to the following piece-square tables.
    */

    val squareValuesPawn = Array(
        Array(  0,  0,  0,  0,  0,  0,  0,  0),
        Array( 50, 50, 50, 50, 50, 50, 50, 50),
        Array( 10, 10, 20, 30, 30, 20, 10, 10),
        Array(  5,  5, 10, 25, 25, 10,  5,  5),
        Array(  0,  0,  0, 20, 20,  0,  0,  0),
        Array(  5, -5,-10,  0,  0,-10, -5,  5),
        Array(  5, 10, 10,-20,-20, 10, 10,  5),
        Array(  0,  0,  0,  0,  0,  0,  0,  0)
    )
    val squareValuesKnight = Array(
        Array(-50,-40,-30,-30,-30,-30,-40,-50),
        Array(-40,-20,  0,  0,  0,  0,-20,-40),
        Array(-30,  0, 10, 15, 15, 10,  0,-30),
        Array(-30,  5, 15, 20, 20, 15,  5,-30),
        Array(-30,  0, 15, 20, 20, 15,  0,-30),
        Array(-30,  5, 10, 15, 15, 10,  5,-30),
        Array(-40,-20,  0,  5,  5,  0,-20,-40),
        Array(-50,-40,-30,-30,-30,-30,-40,-50)
    )
    val squareValuesBishop = Array(
        Array(-20,-10,-10,-10,-10,-10,-10,-20),
        Array(-10,  0,  0,  0,  0,  0,  0,-10),
        Array(-10,  0,  5, 10, 10,  5,  0,-10),
        Array(-10,  5,  5, 10, 10,  5,  5,-10),
        Array(-10,  0, 10, 10, 10, 10,  0,-10),
        Array(-10, 10, 10, 10, 10, 10, 10,-10),
        Array(-10,  5,  0,  0,  0,  0,  5,-10),
        Array(-20,-10,-10,-10,-10,-10,-10,-20)
    )
    val squareValuesRook = Array(
        Array(  0,  0,  0,  0,  0,  0,  0,  0),
        Array(  5, 10, 10, 10, 10, 10, 10,  5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array(  0,  0,  0,  5,  5,  0,  0,  0)
    )
    val squareValuesQueen = Array(
        Array(-20,-10,-10, -5, -5,-10,-10,-20),
        Array(-10,  0,  0,  0,  0,  0,  0,-10),
        Array(-10,  0,  5,  5,  5,  5,  0,-10),
        Array( -5,  0,  5,  5,  5,  5,  0, -5),
        Array(  0,  0,  5,  5,  5,  5,  0, -5),
        Array(-10,  5,  5,  5,  5,  5,  0,-10),
        Array(-10,  0,  5,  0,  0,  0,  0,-10),
        Array(-20,-10,-10, -5, -5,-10,-10,-20)
    )
    val squareValuesKingMid = Array(
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-20,-30,-30,-40,-40,-30,-30,-20),
        Array(-10,-20,-20,-20,-20,-20,-20,-10),
        Array( 20, 20,  0,  0,  0,  0, 20, 20),
        Array( 20, 30, 10,  0,  0, 10, 30, 20)
    )
    val squareValuesKingEnd = Array(
        Array(-50,-40,-30,-20,-20,-30,-40,-50),
        Array(-30,-20,-10,  0,  0,-10,-20,-30),
        Array(-30,-10, 20, 30, 30, 20,-10,-30),
        Array(-30,-10, 30, 40, 40, 30,-10,-30),
        Array(-30,-10, 30, 40, 40, 30,-10,-30),
        Array(-30,-10, 20, 30, 30, 20,-10,-30),
        Array(-30,-30,  0,  0,  0,  0,-30,-30),
        Array(-50,-30,-30,-30,-30,-30,-30,-50)
    )
    val squareValuesDefault = Array.fill(dim_x, dim_y)(0)

    def checkIfIsEndGame(rules:Rules) : Unit =
    {
        var noQueenW = true
        var noQueenB = true
        var nbOtherPiecesW = 0
        var nbOtherPiecesB = 0
        for (i <- 0 to rules.dim_x-1)
        {
            for (j <- 0 to rules.dim_y-1)
            {
                var p = rules.pieceAtPosition(i, j)
                if (p != null)
                {
                    if (p.team == Round.White)
                    {
                        if (p.pieceType == PieceType.Queen)
                            noQueenW = false
                        else
                            nbOtherPiecesW += 1
                    }
                    else if (p.team == Round.Black)
                    {
                        if (p.pieceType == PieceType.Queen)
                            noQueenB = false
                        else
                            nbOtherPiecesB += 1
                    }
                }
            }
        }
        isEndGame = ( (noQueenW && noQueenB) || ((noQueenW || (nbOtherPiecesW <= 3)) && (noQueenB || (nbOtherPiecesB <= 3))) )
    }

    def eval(rules:Rules) : Int =
    {
        printf("Hey\n")
        var res = 0
        var valToAdd = 0
        checkIfIsEndGame(rules)
        for (i <- 0 to rules.dim_x-1)
        {
            for (j <- 0 to rules.dim_y-1)
            {
                printf("Hey_%d_%d\n", i, j)
                var p = rules.pieceAtPosition(i, j)
                if (p != null)
                {
                    valToAdd = pieceVal(p.pieceType)
                    printf("  %d\n", valToAdd)
                    if (p.team == Round.White)
                        valToAdd *= squareVal(p.pieceType)(i)(rules.dim_y - j - 1)
                    else
                    {
                        
                        var tmp = squareVal(p.pieceType)
                        printf("tmp1")
                        var tmp2 = tmp(i)
                        printf("tmp2")
                        printf("  trouvé %d\n", tmp2(j))
                        valToAdd *= squareVal(p.pieceType)(i)(j)
                        printf("  ah non\n")
                    }
                    printf("  inter\n")
                    if (p.team == Round.adv(rules.getRound))
                        valToAdd *= -1
                    printf("  %d\n", valToAdd)
                    res += valToAdd
                }
            }
        }
        return res
    }
}


/**
An Alpha-Beta AI.
*/
class AlphaBetaAI(private val evalFunc:EvalFunc, private val maxDepth:Int) extends SynchPlayer
{
    val minEval = Int.MinValue
    val maxEval = Int.MaxValue

    def computeAlphaBeta(rules:Rules,isMyTurn:Boolean,alpha:Int,beta:Int,prof:Int) : Int =
    {
        if (alpha > beta)
            return (if (isMyTurn) maxEval else minEval)
        else if (prof > maxDepth)
            return evalFunc.eval(rules)
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
