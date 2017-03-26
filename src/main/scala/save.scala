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
    PieceType.King -> "K",
    PieceType.ArchBishop -> "A",
    PieceType.Chancellor -> "C"
  )

  def xToColumn (x:Int) : String =
  {
    return (('a'.toInt + x).toChar).toString
  }
  def yToRow (y:Int) : String =
  {
    return (dim_y - y).toString
  }
  def rowToY (row:String) : Int =
  {
    return dim_y - row.toInt
  }
  def columnToX (col:String) : Int =
  {
    return col(0).toInt - 'a'.toInt
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
  private def isAlphanumeric(c:Char) : Boolean =
  {
    if ('a' <= c && c <= 'z')
      return true
    if ('A' <= c && c <= 'Z')
      return true
    return isNumeric(c)
  }
  private def isNumeric(c:Char) : Boolean =
  {
    if ('0' <= c && c <= '9')
      return true
    return false
  }
  private def pieceOfAbv(abv:String) : PieceType.PieceType =
  {
    pieceTypeAbv.find(_._2==abv) match {
      case None => return PieceType.Unknown
      case Some ((k,v)) => return k
    }
  }
  private def removeIndex(i:Int, str:String) : String =
  {
    return str.substring(0,i)+str.substring(i+1)
  }
  private def removeLast(s:String) : String =
  {
    return s.substring(0, s.length-1)
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
      if (isAlphanumeric(current))
      {
        var content = readUntilSpace(source)
        content = content.split('.').last
        if (!content.isEmpty)
        {
          var castle = CastleType.NoCastle
          var event = GameEvent.NoEvent
          var ptype = PieceType.Unknown
          var fromX = -1 ; var fromY = -1
          var toX = -1 ; var toY = -1
          var isCatch = false ; var promotion = PieceType.Unknown

          // Check/Checkmate annotation
          if (content.endsWith("#"))
              event = GameEvent.Checkmate
          if (content.endsWith("+"))
              event = GameEvent.Check

          // Remove annotations at the end of the move
          while (!isAlphanumeric(content.last))
            content = removeLast(content)
          
          // Castle
          if (current == 'O')
          {
            ptype = PieceType.King
            if (content.startsWith("O-O-O"))
              castle = CastleType.Queenside
            else if (content.startsWith("O-O"))
              castle = CastleType.Kingside
          }
          // Regular move
          else
          {
            // Reading piece type
            ptype = pieceOfAbv(current.toString)
            if (ptype == PieceType.Unknown)
              ptype = PieceType.Pawn
            else
              content = content.substring(1)
            // Promotion ?
            if (content.lastIndexOf('=') >= 0)
              content = removeIndex(content.lastIndexOf('='), content)
            if (!isNumeric(content.last))
            {
              promotion = pieceOfAbv(content.last.toString)
              content = removeLast(content)
            }
            // Catch symbol / No catch symbol
            if (content.indexOf('x') >= 0)
            {
              isCatch = true
              content = removeIndex(content.indexOf('x'), content)
            }
            if (content.indexOf('-') >= 0) // This symbol is not legal in abreged notation, but we tolerate it
              content = removeIndex(content.indexOf('-'), content)
            // TO Position
            var tmp_to_row = ""
            while (isNumeric(content.last))
            {
              tmp_to_row = content.last + tmp_to_row
              content = removeLast(content)
            }
            toY = rowToY(tmp_to_row)
            toX = columnToX(content.last.toString)
            content = removeLast(content)
            // FROM Position
            if (!content.isEmpty)
            {
              if (!isNumeric(content(0)))
              {
                fromX = columnToX(content(0).toString)
                content = content.substring(1)
              }
              if (!content.isEmpty)
              {
                fromY = rowToY(content)
                content = ""
              }
            }
          }
          h.moves.append(new Move(ptype, fromX, fromY, toX, toY, isCatch, castle, promotion, event))
        }
      }
      // Ending (final score) : Nothing for the moment
      // Comments
      if (current == '{')
        readUntilChar(source,'}')
      if (current == ';')
        readUntilChar(source,'\n')

      current = readNextChar(source)
    }
    return h
  }
}
