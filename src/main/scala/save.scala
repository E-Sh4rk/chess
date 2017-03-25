import scala.io.Source
import scala.util.matching.Regex

import java.io.File
import java.io.FileWriter

object CastleType extends Enumeration {
    type CastleType = Value
    val NoCastle, Kingside, Queenside = Value
}

class Move(var pieceType:PieceType.PieceType, /* Always specified */
           var fromX:Int, var fromY:Int, var toX:Int, var toY:Int, /* Negative when not specified */
           var isCatch:Boolean, var isCheck:Boolean, /* Always specified */
           var castle:CastleType.CastleType, var promotion:PieceType.PieceType); /* Always specified */

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

  var dim_x = 8
  var dim_y = 8

  val pieceTypeAbv = scala.collection.mutable.Map[PieceType.PieceType, String]()
  
  def makePGN(fileName:String) =
  {
    
    val fw = new FileWriter(new File(String.format("%s.png", fileName)))
    //fw.write(System.lineSeparator()) //new line
    fw.write(String.format("[Event \"%s\"]\n", event))
    fw.write(String.format("[Site \"%s\"]\n", site))
    fw.write(String.format("[Date \"%s\"]\n", date))
    fw.write(String.format("[Round \"%s\"]\n", round))
    fw.write(String.format("[White \"%s\"]\n", white))
    fw.write(String.format("[Black \"%s\"]\n", black))
    fw.write(String.format("[Result \"%s\"]\n", result))
    fw.write(String.format("[Mode \"%s\"]\n\n", mode.toString()))

    var nbCharsInLine = 0
    /*
    for (move <- moves)
    {
      stringOfMove = ""
      if move.isCastling
      {
        if castlingIsKingside(move)
          stringOfMove = "O-O"
        else
          stringOfMove = "O-O-O"
      }
      else
      {
        if 
      }
    }
    */
    //fw.close()
  }
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