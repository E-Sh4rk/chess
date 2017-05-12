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
    def eval(rules:Rules,team:Round.Round) : Int
}

/**
A dumb evaluation function to check the correctness of the AIs
*/
class EvalToy() extends EvalFunc()
{
    private val pieceVal = scala.collection.mutable.Map[PieceType.PieceType, Int](
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
    def eval(rules:Rules,team:Round.Round) : Int =
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
    private val pieceVal = Map[PieceType.PieceType, Int](
        PieceType.Pawn -> 100,
        PieceType.Knight -> 320,
        PieceType.Bishop -> 330,
        PieceType.Rook -> 500,
        PieceType.Queen -> 900,
        PieceType.King -> 20000,
        PieceType.ArchBishop -> 700,
        PieceType.Chancellor -> 700,
        PieceType.Unknown -> 0
    )
    /*
    Special thanks to : https://chessprogramming.wikispaces.com/Simplified+evaluation+function
    for inspiration to the following piece-square tables.
    */

    private val squareValuesPawn:Array[Array[Int]] = Array(
        Array(  0,  0,  0,  0,  0,  0,  0,  0),
        Array( 50, 50, 50, 50, 50, 50, 50, 50),
        Array( 10, 10, 20, 30, 30, 20, 10, 10),
        Array(  5,  5, 10, 25, 25, 10,  5,  5),
        Array(  0,  0,  0, 20, 20,  0,  0,  0),
        Array(  5, -5,-10,  0,  0,-10, -5,  5),
        Array(  5, 10, 10,-20,-20, 10, 10,  5),
        Array(  0,  0,  0,  0,  0,  0,  0,  0)
    )
    private val squareValuesKnight:Array[Array[Int]] = Array(
        Array(-50,-40,-30,-30,-30,-30,-40,-50),
        Array(-40,-20,  0,  0,  0,  0,-20,-40),
        Array(-30,  0, 10, 15, 15, 10,  0,-30),
        Array(-30,  5, 15, 20, 20, 15,  5,-30),
        Array(-30,  0, 15, 20, 20, 15,  0,-30),
        Array(-30,  5, 10, 15, 15, 10,  5,-30),
        Array(-40,-20,  0,  5,  5,  0,-20,-40),
        Array(-50,-40,-30,-30,-30,-30,-40,-50)
    )
    private val squareValuesBishop:Array[Array[Int]] = Array(
        Array(-20,-10,-10,-10,-10,-10,-10,-20),
        Array(-10,  0,  0,  0,  0,  0,  0,-10),
        Array(-10,  0,  5, 10, 10,  5,  0,-10),
        Array(-10,  5,  5, 10, 10,  5,  5,-10),
        Array(-10,  0, 10, 10, 10, 10,  0,-10),
        Array(-10, 10, 10, 10, 10, 10, 10,-10),
        Array(-10,  5,  0,  0,  0,  0,  5,-10),
        Array(-20,-10,-10,-10,-10,-10,-10,-20)
    )
    private val squareValuesRook:Array[Array[Int]] = Array(
        Array(  0,  0,  0,  0,  0,  0,  0,  0),
        Array(  5, 10, 10, 10, 10, 10, 10,  5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array( -5,  0,  0,  0,  0,  0,  0, -5),
        Array(  0,  0,  0,  5,  5,  0,  0,  0)
    )
    private val squareValuesQueen:Array[Array[Int]] = Array(
        Array(-20,-10,-10, -5, -5,-10,-10,-20),
        Array(-10,  0,  0,  0,  0,  0,  0,-10),
        Array(-10,  0,  5,  5,  5,  5,  0,-10),
        Array( -5,  0,  5,  5,  5,  5,  0, -5),
        Array(  0,  0,  5,  5,  5,  5,  0, -5),
        Array(-10,  5,  5,  5,  5,  5,  0,-10),
        Array(-10,  0,  5,  0,  0,  0,  0,-10),
        Array(-20,-10,-10, -5, -5,-10,-10,-20)
    )
    private val squareValuesKingMid:Array[Array[Int]] = Array(
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-20,-30,-30,-40,-40,-30,-30,-20),
        Array(-10,-20,-20,-20,-20,-20,-20,-10),
        Array( 20, 20,  0,  0,  0,  0, 20, 20),
        Array( 20, 30, 10,  0,  0, 10, 30, 20)
    )
    private val squareValuesKingEnd:Array[Array[Int]] = Array(
        Array(-50,-40,-30,-20,-20,-30,-40,-50),
        Array(-30,-20,-10,  0,  0,-10,-20,-30),
        Array(-30,-10, 20, 30, 30, 20,-10,-30),
        Array(-30,-10, 30, 40, 40, 30,-10,-30),
        Array(-30,-10, 30, 40, 40, 30,-10,-30),
        Array(-30,-10, 20, 30, 30, 20,-10,-30),
        Array(-30,-30,  0,  0,  0,  0,-30,-30),
        Array(-50,-30,-30,-30,-30,-30,-30,-50)
    )
    private val squareValuesDefault:Array[Array[Int]] = Array.fill(8, 8)(0)

    private val squareValMid = Map[PieceType.PieceType, Array[Array[Int]]] (
        PieceType.Pawn -> squareValuesPawn,
        PieceType.Knight -> squareValuesKnight,
        PieceType.Bishop -> squareValuesBishop,
        PieceType.Rook -> squareValuesRook,
        PieceType.Queen -> squareValuesQueen,
        PieceType.King -> squareValuesKingMid,
        PieceType.ArchBishop -> squareValuesDefault,
        PieceType.Chancellor -> squareValuesDefault,
        PieceType.Unknown -> squareValuesDefault
    )
    private val squareValEnd = Map[PieceType.PieceType, Array[Array[Int]]] (
        PieceType.Pawn -> squareValuesPawn,
        PieceType.Knight -> squareValuesKnight,
        PieceType.Bishop -> squareValuesBishop,
        PieceType.Rook -> squareValuesRook,
        PieceType.Queen -> squareValuesQueen,
        PieceType.King -> squareValuesKingEnd,
        PieceType.ArchBishop -> squareValuesDefault,
        PieceType.Chancellor -> squareValuesDefault,
        PieceType.Unknown -> squareValuesDefault
    )

    private def isEndGame(rules:Rules) : Boolean =
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
        return (noQueenW && noQueenB) || ((noQueenW || nbOtherPiecesW <= 3) && (noQueenB || nbOtherPiecesB <= 3))
    }

    def eval(rules:Rules,team:Round.Round) : Int =
    {
        var res = 0
        var squareVal = squareValMid
        if (isEndGame(rules))
            squareVal = squareValEnd
        val x_diff = (rules.dim_x - 8)/2
        val y_diff = (rules.dim_y - 8)/2

        for (i <- 0 to rules.dim_x-1)
        {
            for (j <- 0 to rules.dim_y-1)
            {
                var p = rules.pieceAtPosition(i, j)
                if (p != null)
                {
                    // Converting x,y positions if chessboard has not standard dimensions
                    var i2 = i-x_diff
                    var j2 = j-y_diff
                    if (i2 < 0)
                        i2 = 0
                    if (i2 > 7)
                        i2 = 7
                    if (j2 < 0)
                        j2 = 0
                    if (j2 > 7)
                        j2 = 7
                        
                    var valToAdd = 0
                    valToAdd = pieceVal(p.pieceType)
                    if (p.team == Round.White)
                        valToAdd += squareVal(p.pieceType)(j2)(i2)
                    else
                        valToAdd += squareVal(p.pieceType)(rules.dim_y - j2 - 1)(i2)
                    if (p.team != team)
                        valToAdd *= -1
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
    private val minEval = Int.MinValue
    private val maxEval = Int.MaxValue

    private def computeAlphaBeta(rules:Rules,isMyTurn:Boolean,alpha:Int,beta:Int,prof:Int) : Int =
    {
        if (prof > maxDepth)
            return evalFunc.eval(rules,if (isMyTurn) rules.getRound else Round.adv(rules.getRound))
        else
        {
            var moves = rules.possibleMoves
            var bestVal = (if (isMyTurn) minEval else maxEval)
            var alphaCurrent = alpha
            var betaCurrent = beta
            for ((fromX, fromY, toX, toY) <- moves)
            {
                var rulesTest = new Rules(rules)
                rulesTest.move(fromX, fromY, toX, toY)
                var valTest = computeAlphaBeta(rulesTest, !isMyTurn, alphaCurrent, betaCurrent, prof + 1)
                if (isMyTurn)
                {
                    if (valTest > bestVal)
                        bestVal = valTest
                    if (bestVal > alphaCurrent)
                        alphaCurrent = bestVal
                    if (alphaCurrent >= betaCurrent)
                        return bestVal
                }
                else
                {
                    if (valTest < bestVal)
                        bestVal = valTest
                    if (bestVal < betaCurrent)
                        betaCurrent = bestVal
                    if (alphaCurrent >= betaCurrent)
                        return bestVal
                }
            }
            return bestVal
        }
    }
    
    private def alphaBeta(rules:Rules) : (Int,Int,Int,Int) =
    {
        val moves = rules.possibleMoves
        var moveChosen = moves.head
        var moveChosenValue = minEval
        for (moveTest <- moves)
        {
            var moveTestValue = 0
            val (fromX, fromY, toX, toY) = moveTest
            var rulesTest = new Rules(rules)
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
