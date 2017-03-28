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

  private def xToColumn (x:Int) : String =
  {
    if (x >= dim_x || x < 0)
      throw new Exception("Wrong x dim !")
    return (('a'.toInt + x).toChar).toString
  }
  private def yToRow (y:Int) : String =
  {
    if (y >= dim_y || y < 0)
      throw new Exception("Wrong y dim !")
    return (dim_y - y).toString
  }
  private def rowToY (row:String) : Int =
  {
    val res = dim_y - row.toInt
    if (res < 0 || res >= dim_y)
      throw new Exception("Wrong row !")
    return res
  }
  private def columnToX (col:Char) : Int =
  {
    val res = col.toInt - 'a'.toInt
    if (res < 0 || res >= dim_x)
      throw new Exception("Wrong column !")
    return res
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
    fw.write(String.format("[Mode \"%s\"]\n", mode.toString()))
    fw.write(String.format("[DimX \"%s\"]\n", dim_x.toString()))
    fw.write(String.format("[DimY \"%s\"]\n\n", dim_y.toString()))

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
        stringOfMove += History.pieceTypeAbv(move.pieceType)
        if (move.fromX >= 0)
          stringOfMove += xToColumn(move.fromX)
        if (move.fromY >= 0)
          stringOfMove += yToRow(move.fromY)
        if (move.isCatch)
          stringOfMove += "x"
        stringOfMove += xToColumn(move.toX)
        stringOfMove += yToRow(move.toY)
        if (move.promotion != PieceType.Unknown)
          stringOfMove += "=" + History.pieceTypeAbv(move.promotion)
      }
      if (move.event == GameEvent.Check)
        stringOfMove += "+"
      if (move.event == GameEvent.Checkmate)
        stringOfMove += "#"

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
}

object History
{
  private val pieceTypeAbv = scala.collection.mutable.Map[PieceType.PieceType, String](
    PieceType.Pawn -> "",
    PieceType.Rook -> "R",
    PieceType.Knight -> "N",
    PieceType.Bishop -> "B",
    PieceType.Queen -> "Q",
    PieceType.King -> "K",
    PieceType.ArchBishop -> "A",
    PieceType.Chancellor -> "C"
  )
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
  private def pieceOfAbv(abv:Char) : PieceType.PieceType =
  {
    pieceTypeAbv.find(_._2==abv.toString) match {
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
          case "dimx" => h.dim_x = tagValue.toInt
          case "dimy" => h.dim_y = tagValue.toInt
          case _ => { }
        }
      }
      // Ending (unknown final score)
      if (current == '*')
      {
        // Nothing for the moment...
      }
      if (isAlphanumeric(current))
      {
        var content = current.toString + readUntilSpace(source)
        // Removing the 'en passant' indicator (not legal in PGN but we tolerate it)
        content = content.replace("e.p.", "")
        // Ending (known final score)
        if (content == "1-0" || content == "0-1" || content == "1/2-1/2" || content == "0,5-0,5" || content.isEmpty)
        {
          // Nothing for the moment...
        }
        // Moves
        else if (content.last != '.') // If it is just the round number, we don't mind
        {
          content = content.split('.').last
          var castle = CastleType.NoCastle
          var event = GameEvent.NoEvent
          var ptype = PieceType.Unknown
          var fromX = -1 ; var fromY = -1
          var toX = -1 ; var toY = -1
          var isCatch = false ; var promotion = PieceType.Unknown

          // Check/Checkmate annotation
          if (content.contains("+"))
              event = GameEvent.Check
          if (content.contains("#"))
              event = GameEvent.Checkmate

          // Remove annotations at the end of the move
          var c = content.last
          while (!isAlphanumeric(c))
          {
            content = removeLast(content)
            if (!content.isEmpty)
              c = content.last
            else
              c = '0'
          }
          
          // Castle
          if (content.contains("O-O"))
          {
            ptype = PieceType.King
            castle = CastleType.Kingside
            if (content.contains("O-O-O"))
              castle = CastleType.Queenside
          }
          // Regular move
          else if (!content.isEmpty)
          {
            // Reading piece type
            ptype = pieceOfAbv(content(0))
            if (ptype == PieceType.Unknown)
              ptype = PieceType.Pawn
            else
              content = content.substring(1)
            // Promotion ?
            if (!isNumeric(content.last))
            {
              promotion = pieceOfAbv(content.last)
              if (promotion != PieceType.Unknown)
                content = removeLast(content)
            }
            if (content.last == '=')
              content = removeLast(content)
            // TO Position
            var to_row_str = ""
            while (isNumeric(content.last))
            {
              to_row_str = content.last + to_row_str
              content = removeLast(content)
            }
            toY = h.rowToY(to_row_str)
            toX = h.columnToX(content.last)
            content = removeLast(content)
            // Catch symbol / No catch symbol
            if (!content.isEmpty)
            {
              if (content.last == 'x')
              {
                isCatch = true
                content = removeLast(content)
              }
              else if (content.last == '-') // This symbol is not legal in PGN, but we tolerate it
                content = removeLast(content)
            }
            // FROM Position
            if (!content.isEmpty)
            {
              if (!isNumeric(content(0)))
              {
                fromX = h.columnToX(content(0))
                content = content.substring(1)
              }
              if (!content.isEmpty)
              {
                fromY = h.rowToY(content)
                content = ""
              }
            }
          }
          h.moves.append(new Move(ptype, fromX, fromY, toX, toY, isCatch, castle, promotion, event))
        }
      }
      // Comments
      if (current == '{')
        readUntilChar(source,'}')
      if (current == ';')
        readUntilChar(source,'\n')
      if (current == '%') // % is used for extensions of PGN
        readUntilChar(source,'\n')
      if (current == '(') // (...) is used for variants
        readUntilChar(source,')')
      if (current == '$') // $ is used for NAG
        readUntilSpace(source)

      current = readNextChar(source)
    }
    return h
  }
}
