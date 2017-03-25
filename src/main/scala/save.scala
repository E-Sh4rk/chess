import scala.io.Source
import scala.util.matching.Regex

import java.io.File
import java.io.FileWriter

object CastleType extends Enumeration {
    type CastleType = Value
    val NoCastle, Kingside, Queenside = Value
}

object GameEvent extends Enumeration {
    type GameEvent = Value
    val NoEvent, Check, Checkmate = Value
}

class Move(var pieceType:PieceType.PieceType, /* Always specified */
           var fromX:Int, var fromY:Int, var toX:Int, var toY:Int, /* Negative when not specified */
           var isCatch:Boolean, var castle:CastleType.CastleType, var promotion:PieceType.PieceType, var event:GameEvent.GameEvent); /* Always specified */

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

  val pieceTypeAbv = scala.collection.mutable.Map[PieceType.PieceType, String](
    PieceType.Pawn -> "",
    PieceType.Rook -> "R",
    PieceType.Knight -> "N",
    PieceType.Bishop -> "B",
    PieceType.Queen -> "Q",
    PieceType.King -> "K"
  )

  def xToColumn (x:Int) : String =
  {
    return (('a'.toInt + x).toChar).toString
  }
  def yToRow (y:Int) : String =
  {
    return (dim_y - y).toString
  }

  def savePGN(fileName:String) =
  {
    val fw = new FileWriter(new File(String.format("%s", fileName)))
    fw.write(String.format("[Event \"%s\"]\n", event))
    fw.write(String.format("[Site \"%s\"]\n", site))
    fw.write(String.format("[Date \"%s\"]\n", date))
    fw.write(String.format("[Round \"%s\"]\n", round))
    fw.write(String.format("[White \"%s\"]\n", white))
    fw.write(String.format("[Black \"%s\"]\n", black))
    fw.write(String.format("[Result \"%s\"]\n", result))
    fw.write(String.format("[Mode \"%s\"]\n\n", mode.toString()))

    var nbCharsInLine = 0
    var roundNumber = 0
    for (move <- moves)
    {
      var stringOfMove = ""

      // Round number
      if (roundNumber%2 == 0)
        stringOfMove += (roundNumber/2 + 1).toString + ". "
      
      // Move
      if (move.castle == CastleType.Kingside)
        stringOfMove += "O-O"
      else if (move.castle == CastleType.Queenside)
        stringOfMove += "O-O-O"
      else
      {
        stringOfMove += pieceTypeAbv(move.pieceType)
        if (move.fromX >= 0)
          stringOfMove += xToColumn(move.fromX)
        if (move.fromY >= 0)
          stringOfMove += yToRow(move.fromY)
        if (move.isCatch)
          stringOfMove += "x"
        stringOfMove += xToColumn(move.toX)
        stringOfMove += yToRow(move.toY)
        if (move.promotion != PieceType.Unknown)
          stringOfMove += "=" + pieceTypeAbv(move.promotion)
        if (move.event == GameEvent.Check)
          stringOfMove += "+"
        if (move.event == GameEvent.Checkmate)
          stringOfMove += "#"
      }

      // Add move to the file
      stringOfMove += " "
      if (nbCharsInLine + stringOfMove.length > 80)
      {
        fw.write(System.lineSeparator())
        nbCharsInLine = 0
      }
      fw.write(stringOfMove)
      nbCharsInLine += stringOfMove.length
      roundNumber += 1
    }
    fw.write(System.lineSeparator())

    // Terminateur : 1/2-1/2 ou 1-0 ou 0-1 ou *
    fw.write("*")
    fw.write(System.lineSeparator())
    
    fw.close()
  }

  private def readNextChar(s:Source) : Char =
  {
    while (s.hasNext)
    {
      val c = s.next
      if (c != '\n' && c != ' ' && c != '\r' && c != '\t')
        return c
    }
    return '\0'
  }
  private def readUntilChar(s:Source, d:Char) : String =
  {
    var res = new StringBuilder
    while (s.hasNext)
    {
      val c = s.next
      if (c == d)
        return res.toString
      res.append(c)
    }
    return res.toString
  }
  private def readUntilSpace(s:Source) : String =
  {
    var res = new StringBuilder
    while (s.hasNext)
    {
      val c = s.next
      if (c == '\n' || c == ' ' || c == '\r' || c == '\t')
        return res.toString
      res.append(c)
    }
    return res.toString
  }
  def loadPGN(fileName:String) : History =
  {
    var h = new History
    var source = Source.fromFile(fileName)
    var current = readNextChar(source)
    while (current != '\0')
    {
      // Header
      if (current == '[')
      {
        var content = readUntilChar(source,']')
        var tagName = content.split('"')(0)
        var tagValue = content.split('"')(1)
        tagName.filterNot((x: Char) => x.isWhitespace).toLowerCase match
        {
          case "event" => h.event = tagValue
          case "site" => h.site = tagValue
          case "date" => h.date = tagValue
          case "round" => h.round = tagValue
          case "white" => h.white = tagValue
          case "black" => h.black = tagValue
          case "result" => h.result = tagValue
          case "mode" => h.mode = GameMode.withName(tagValue)
          case _ => { }
        }
      }
      // Moves
      {
        // Castle
        if (current == 'O')
        {
          var castle = CastleType.NoCastle
          var event = GameEvent.NoEvent
          var content = readUntilSpace(source)
          if (content.startsWith("O-O-O"))
            castle = CastleType.Queenside
          else if (content.startsWith("O-O"))
            castle = CastleType.Kingside
          if (content.endsWith("#"))
            event = GameEvent.Checkmate
          if (content.endsWith("+"))
            event = GameEvent.Check
          h.moves.append(new Move(PieceType.King, -1, -1, -1, -1, false, castle, PieceType.Unknown, event))
        }
        // TODO
      }
      // Ending (final score) : Nothing for the moment
      current = readNextChar(source)
    }
    return h
  }
}
