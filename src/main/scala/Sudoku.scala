package Games

import java.awt.*
import javax.swing.*
import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.util.Random.{between, nextInt}
import scala.util.matching.Regex
import java.io.PrintWriter
import org.jpl7._

def SudokuInitializeGrid(): Array[Array[String]] = {
  @tailrec
  def randomCell(grid: Array[Array[String]], exist: Array[Array[Array[Boolean]]], i: Int, j: Int, num: Int): Unit = {
    exist(2)(i / 3 * 3 + j / 3)(num) match {
      case true => randomCell(grid, exist, i, j, between(1, 10))
      case false => grid(i).update(j, "r".concat(num.toString))
        exist(0)(i).update(num, true)
        exist(1)(j).update(num, true)
        exist(2)(i / 3 * 3 + j / 3).update(num, true)
    }
  }

  def fillDiagonalBox(row: Int, col: Int, grid: Array[Array[String]], exist: Array[Array[Array[Boolean]]]): Unit = {
    Range(row, row + 3).foreach { i =>
      Range(col, col + 3).foreach { j =>
        randomCell(grid, exist, i, j, between(1, 10))
      }
    }
  }

  def fillRecursion(cell: Int, grid: Array[Array[String]], exist: Array[Array[Array[Boolean]]]): Boolean = cell match {
    case 81 => true
    case c if grid(cell / 9)(cell % 9).startsWith("r") => fillRecursion(c + 1, grid, exist)
    case _ =>
      Range(1, 10, 1)
      .filter(i => !exist(0)(cell / 9)(i) && !exist(1)(cell % 9)(i) && !exist(2)(cell / 9 / 3 * 3 + cell % 9 / 3)(i))
      .map(i => {
        exist(0)(cell / 9).update(i, true)
        exist(1)(cell % 9).update(i, true)
        exist(2)(cell / 9 / 3 * 3 + cell % 9 / 3).update(i, true)
        val solved = fillRecursion(cell + 1, grid, exist)
        exist(0)(cell / 9).update(i, false)
        exist(1)(cell % 9).update(i, false)
        exist(2)(cell / 9 / 3 * 3 + cell % 9 / 3).update(i, false)
        (solved, i)
      })
      .find(_._1)
      .map { (_, num) =>
        grid(cell / 9).update(cell % 9, "r".concat(num.toString))
        true
      }.getOrElse(false)
  }

  def deleteRandomCells(grid: Array[Array[String]]): Unit = {
    grid.indices.foreach(i => grid(i).indices.foreach(j => {
      grid(i).update(j, if ((between(1, 11) compare 4).equals(1)) "b " else grid(i)(j))
    }))
  }

  def randomGrid(grid: Array[Array[String]], exist: Array[Array[Array[Boolean]]]): Array[Array[String]] = {
    Range(0, 9, 3).foreach(i => fillDiagonalBox(i, i, grid, exist))
    fillRecursion(0,grid, exist)
    deleteRandomCells(grid)
    grid
  }
  randomGrid(Array.fill(9, 9)("b "), Array.fill(3, 9, 10)(false))
}

def SudokuDrawer(grid: Array[Array[String]]): Array[Array[String]] = {
  val frame = new JFrame("Board Drawing Game")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(640, 640)
  val panel = new JPanel(new GridLayout(3, 3, 2, 2))
  panel.setSize(600, 600)
  panel.setBackground(Color.BLACK)
  val lettersPanel = new JPanel(new GridLayout(1, 10))
  List("a", "b", "c", "d", "e", "f", "g", "h", "i").foreach {
    letter => lettersPanel.add(new JLabel(letter, SwingConstants.CENTER))
  }
  val numbersPanel = new JPanel(new GridLayout(9, 1))
  List(1, 2, 3, 4, 5, 6, 7, 8, 9).reverse.foreach {
    number => numbersPanel.add(new JLabel(number.toString, SwingConstants.CENTER))
  }
  val panelsList: List[JPanel] = List.fill(9)(new JPanel(new GridLayout(3, 3)))
  val buttonsList: List[List[JButton]] = List.fill(9, 9)(new JButton())
  buttonsList.zipWithIndex.foreach { case (row, i) =>
    row.zipWithIndex.foreach { case (button, j) =>
      button.setText(grid(8 - i)(j).substring(1))
      button.setForeground(grid(8 - i)(j).charAt(0) match {
        case 'r' => Color.RED
        case _ => Color.BLACK
      })
      button.setBackground(Color.WHITE)
      button.setFont(new Font("Arial", Font.BOLD, 40))
      panelsList(i / 3 * 3 + j / 3).add(button)
    }
  }
  panelsList.foreach(panel.add)
  frame.getContentPane.add(panel, BorderLayout.CENTER)
  frame.add(lettersPanel, BorderLayout.SOUTH)
  frame.add(numbersPanel, BorderLayout.WEST)
  frame.setVisible(true)
  grid
}

def SudokuController(move: String, grid: Array[Array[String]], player: Int): Any = {

  def SudokuUpdateGrid(row: Int, col: Int, grid: Array[Array[String]], num: String): Array[Array[String]] = {
    grid(row).update(col, "b".concat(num))
    grid
  }

  def isValidPlacement(grid: Array[Array[String]], row: Int, col: Int, num: Char): Boolean = {
    !List(grid(row).toList.flatten, grid.flatMap(row => row(col)).toList, grid
      .slice(row / 3 * 3, row / 3 * 3 + 3)
      .flatMap(row => row.slice(col / 3 * 3, col / 3 * 3 + 3)).flatten.toList)
      .flatten.contains(num)
  }

  def SudokuIsValid(move: String, grid: Array[Array[String]], insertPattern: Regex = """([1-9])([a-i]) ([1-9])""".r, removePattern: Regex = """([1-9])([a-i])""".r): Boolean = move match {
    case insertPattern(row, col, num) => grid(row(0).asDigit - 1)(map(col(0))) match
      case "b " => isValidPlacement(grid, row(0).asDigit - 1, map(col(0)), num(0))
      case _ => false
    case removePattern(row, col) => !grid(row(0).asDigit - 1)(map(col(0)))(0).equals('r')
      && !grid(row(0).asDigit - 1)(map(col(0)))(1).equals(' ')
    case _ => false
  }

  def saveGrid(grid: Array[Array[String]]): Unit = {
    val writer = new PrintWriter("sudoku.txt")
    try {
      grid.reverse.foreach { row =>
        row.foreach {
          cell => writer.print((if (cell(1).equals(' ')) "0" else cell(1).toString).concat(" "))
        }
        writer.println()
      }
    } finally writer.close()
  }
  
  move match {
    case "solve" => saveGrid(grid)
      new Query("consult('SudokuSolver.pl')").nextSolution
      val query = Query("read_and_solve_sudoku('sudoku.txt', Solution).")
      query.hasNext match {
        case true => query.nextSolution.get("Solution").asInstanceOf[Compound].toString
          .stripPrefix("[[").stripSuffix("]]").split("\\], \\[")
          .map(innerArray => innerArray.split(", ").map(_.trim))
          .reverse.zipWithIndex.foreach { case (row, i) =>
          row.zipWithIndex.foreach { case (cell, j) =>
            grid(i).update(j, grid(i)(j).substring(0, 1).concat(cell))
            }
          }
          grid
        case false => notSolvableFunction
      }
    case _ => SudokuIsValid (move, grid) match
      case false => invalid1PlayerFunction
      case true => SudokuUpdateGrid (move.charAt (0).asDigit - 1, map (move.charAt (1) ), grid, if (move.length == 4) move.substring (3) else " ")
  }
}