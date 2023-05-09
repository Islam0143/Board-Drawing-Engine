package Games

import java.awt.*
import javax.swing.*
import scala.collection.immutable.{HashMap, List}
import scala.util.matching.Regex

def ChessValidChar(ch: Char): Boolean = "abcdefgh".toList.contains(ch)
def ChessValidDigit(dig: Char): Boolean = "12345678".toList.contains(dig)

def ChessInitializeGrid(): Array[Array[String]] = {
  Array.fill(8, 8)(" ")
    .updated(0, Array("w\u265C", "w\u265E", "w\u265D", "w\u265B", "w\u265A", "w\u265D", "w\u265E", "w\u265C"))
    .updated(1, Array.fill(8)("w\u265F"))
    .updated(6, Array.fill(8)("b\u265F"))
    .updated(7, Array("b\u265C", "b\u265E", "b\u265D", "b\u265B", "b\u265A", "b\u265D", "b\u265E", "b\u265C"))
}

def ChessDrawer(grid: Array[Array[String]], frame: JFrame, panel: JPanel): Array[Array[String]] = {
  val buttonsList: List[List[JButton]] = List.fill(8, 8)(new JButton())
  panel.removeAll()
  panel.setLayout(new GridLayout(8, 8))
  val lettersPanel = new JPanel(new GridLayout(1, 9))
  "abcdefgh".toList.foreach {
    letter => lettersPanel.add(new JLabel(letter.toString, SwingConstants.CENTER))
  }
  val numbersPanel = new JPanel(new GridLayout(8, 1))
  "12345678".toList.reverse.foreach {
    number => numbersPanel.add(new JLabel(number.toString, SwingConstants.CENTER))
  }
  buttonsList.zipWithIndex.foreach { case (row, i) =>
    row.zipWithIndex.foreach { case (button, j) =>
      button.setText(grid(i)(j).substring(1))
      button.setBackground(if ((i + j) % 2 == 0) Color.GRAY else Color.blue)
      button.setForeground(if (grid(i)(j).charAt(0).equals('w')) Color.WHITE else Color.BLACK)
      button.setFont(new Font("Arial Unicode MS", Font.PLAIN, 26))
    }
  }
  buttonsList.reverse.foreach(row => row.foreach(panel.add))
  frame.getContentPane.add(panel, BorderLayout.CENTER)
  frame.add(lettersPanel, BorderLayout.SOUTH)
  frame.add(numbersPanel, BorderLayout.WEST)
  frame.setVisible(true)
  grid
}

def ChessController(move: String, grid: Array[Array[String]], player: Int): Any = {

  def ChessColor(player: Int): Char = if (player == 1) 'w' else 'b'

  def ChessUpdateGrid(row1: Int, col1: Int, row2: Int, col2: Int, grid: Array[Array[String]]): Array[Array[String]] = {
    grid(row2).update(col2, grid(row1)(col1))
    grid(row1).update(col1, " ")
    grid
  }

  def ChessIsValid(move: String, grid: Array[Array[String]], player: Int, inputPattern: Regex): Boolean = move match {
    case inputPattern(row1, col1, row2, col2) => grid(row1(0).asDigit - 1)(map(col1(0))) match {
      case " " => false
      case piece => piece(0).equals(ChessColor(player)) &&
        !grid(row2(0).asDigit - 1)(map(col2(0)))(0).equals(ChessColor(player)) &&
        canMove(piece.substring(1), grid, ChessColor(player))(row1(0).asDigit - 1, map(col1(0)), row2(0).asDigit - 1, map(col2(0)))
    }
    case _ => false
  }

  def canMove(piece: String, grid: Array[Array[String]], color: Char): (Int, Int, Int, Int) => Boolean = piece match {
    case "\u265F" => checkPawn(grid, pawnDirection(color), _, _, _, _)
    case "\u265C" => checkCastle(grid, _, _, _, _)
    case "\u265E" => checkKnight
    case "\u265D" => checkBishop(grid, _, _, _, _)
    case "\u265B" => checkQueen(grid, _, _, _, _)
    case "\u265A" => checkKing
  }

  def pawnDirection(color: Char): Int = if (color.equals('w')) 1 else -1
  def pawnStartRow(pawnDirection: Int): Int = if (pawnDirection == 1) 1 else 6

  def checkPawn(grid: Array[Array[String]], pawnDirection: Int, row1: Int, col1: Int, row2: Int, col2: Int): Boolean = (col1 == col2, row2 - row1) match {
    case (true, `pawnDirection`) => grid(row2)(col2).equals(" ")
    case (true, n) if n == 2 * pawnDirection && row1 == pawnStartRow(pawnDirection) =>
      noPieceVerticallyBetween(row1, col1, row2, col2, grid) && grid(row2)(col2).equals(" ")
    case (false, `pawnDirection`) if Math.abs(col1 - col2) == 1 => grid(row2)(col2).startsWith(if (pawnDirection == 1) "b" else "w")
    case _ => false
  }

  def checkCastle(grid: Array[Array[String]], row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    isVertical(row1, col1, row2, col2) && noPieceVerticallyBetween(row1, col1, row2, col2, grid) ||
      isHorizontal(row1, col1, row2, col2) && noPieceHorizontallyBetween(row1, col1, row2, col2, grid)

  def checkKnight(row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    List((1, 2), (2, 1)).contains((Math.abs(row1 - row2), Math.abs(col1 - col2)))

  def checkBishop(grid: Array[Array[String]], row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    isDiagonal(row1, col1, row2, col2) && noPieceDiagonallyBetween(row1, col1, row2, col2, grid)

  def checkQueen(grid: Array[Array[String]], row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    checkCastle(grid, row1, col1, row2, col2) || checkBishop(grid, row1, col1, row2, col2)

  def checkKing(row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    Math.abs(row1 - row2) == 1 || Math.abs(col1 - col2) == 1

  def isVertical(row1: Int, col1: Int, row2: Int, col2: Int): Boolean = row1 != row2 && col1 == col2
  def isHorizontal(row1: Int, col1: Int, row2: Int, col2: Int): Boolean = row1 == row2 && col1 != col2
  def isDiagonal(row1: Int, col1: Int, row2: Int, col2: Int): Boolean = Math.abs(row1 - row2) == Math.abs(col1 - col2)

  def noPieceVerticallyBetween(row1: Int, col1: Int, row2: Int, col2: Int, grid: Array[Array[String]]): Boolean = {
    Range(Math.min(row1, row2) + 1, Math.max(row1, row2))
      .forall(cell => grid(cell)(col1).equals(" "))
  }

  def noPieceHorizontallyBetween(row1: Int, col1: Int, row2: Int, col2: Int, grid: Array[Array[String]]): Boolean = {
    Range(Math.min(col1, col2) + 1, Math.max(col1, col2))
      .forall(cell => grid(row1)(cell).equals(" "))
  }

  def noPieceDiagonallyBetween(row1: Int, col1: Int, row2: Int, col2: Int, grid: Array[Array[String]]): Boolean = {
    Range(row1 + Math.abs(row1 - row2) - Math.abs(row2 - row1 - 1), row2, if (row1 > row2) -1 else 1)
      .zip(Range(col1 + Math.abs(col1 - col2) - Math.abs(col2 - col1 - 1), col2, if (col1 > col2) -1 else 1))
      .forall { case (i, j) => grid(i)(j).equals(" ") }
  }

  ChessIsValid(move, grid, player, """([1-9])([a-i]) ([1-9])([a-i])""".r) match
    case false => invalid2Players(player)
    case true => ChessUpdateGrid(move.charAt(0).asDigit - 1, map(move.charAt(1)), move.charAt(3).asDigit - 1, map(move.charAt(4)), grid)
}