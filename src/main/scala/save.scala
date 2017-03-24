import scala.io.Source
import scala.util.matching.Regex

class Move(val pieceType:PieceType.PieceType,
           val fromX:Int, val fromY:Int, val toX:Int, val toY:Int,
           val isCatch:Boolean, val isCheck:Boolean,
           val isCastling:Boolean, val isPromotion:Boolean);

class History()
{
  var moves = new scala.collection.mutable.ListBuffer[Move]()
  var event = "?"
  var site = "?"
  var date = "????.??.??"
  var round = "?"
  var white = "?"
  var black = "?"
  var result = "*"
  var mode = GameMode.Vanilla
}


// Source.fromFile("mon/fichier.txt").getLines
/*
object Demo {
   def main(args: Array[String]) {
      val pattern = "\\[Scala".r
      val str = "[Scala is Scalable and cool"
      
      println((pattern findAllIn str).mkString(""))
   }
}
*/