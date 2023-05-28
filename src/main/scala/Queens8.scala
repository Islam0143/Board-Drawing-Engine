package Games

import java.awt._
import javax.swing._
import scala.collection.immutable.List
import org.jpl7._
def Queens8InitializeGrid(): Array[Array[String]] = Array.fill(8, 8)(" ")

def Queens8Drawer(grid: Array[Array[String]]): Array[Array[String]] = {
  val buttonsList: List[List[JButton]] = List.fill(8, 8)(new JButton())
  val frame = new JFrame("Board Drawing Game")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(500, 500)
  val panel = new JPanel(new GridLayout(8, 8))
  panel.setSize(480, 480)
  val lettersPanel = new JPanel(new GridLayout(1, 9))
  List("a", "b", "c", "d", "e", "f", "g", "h").foreach {
    letter => lettersPanel.add(new JLabel(letter, SwingConstants.CENTER))
  }
  val numbersPanel = new JPanel(new GridLayout(8, 1))
  List(1, 2, 3, 4, 5, 6, 7, 8).reverse.foreach {
    number => numbersPanel.add(new JLabel(number.toString, SwingConstants.CENTER))
  }
  buttonsList.zipWithIndex.foreach { case (row, i) =>
    row.zipWithIndex.foreach { case (button, j) =>
      button.setText(grid(i)(j))
      button.setBackground(if ((i + j) % 2 == 0) Color.GRAY else Color.blue)
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

def Queens8Controller(move: String, grid: Array[Array[String]], player: Int): Any = {
  def Queens8ValidChar(ch: Char): Boolean = ChessValidChar(ch)
  def Queens8ValidDigit(dig: Char): Boolean = ChessValidDigit(dig)
  def Queens8UpdateGrid(row: Int, col: Int, grid: Array[Array[String]]): Array[Array[String]] = {
    grid(row).update(col, if(grid(row)(col).equals(" ")) "\u265B" else " ")
    grid
  }
  def Queens8IsValid(move: String, grid: Array[Array[String]]): Boolean = {
    move.length == 2 && ( move.toList.zipWithIndex match {
      case List((row, 0), (col, 1)) => Queens8ValidDigit(row) && Queens8ValidChar(col)
        && ( grid(row.asDigit - 1)(map(col)) match
        case " " => !grid.flatten.zipWithIndex.filter(x => x._2 / 8 == row.asDigit-1
          || x._2 % 8 == map(col)
          || x._2 / 8 + x._2 % 8 == row.asDigit - 1 + map(col)
          || x._2 / 8 - x._2 % 8 == row.asDigit - 1 - map(col) )
          .map(_._1).contains("\u265B")
        case _ => true
        )
      case _ => false
    })
  }
  
  move match {
    case "solve" => new Query("consult('Queens8Solver.pl')").nextSolution
      val Queens = grid.transpose.map(col => if (col.contains("\u265B")) col.indexOf("\u265B") + 1 else "_").mkString("[", ", ", "]")
      val query = Query(s"Solution = $Queens, queens8(Solution).")
      query.hasNext match {
        case true => query.nextSolution.get("Solution").asInstanceOf[Compound].toString
          .stripPrefix("[").stripSuffix("]")
          .split(", ").map(_.toInt).toList
          .zipWithIndex.foreach { case (row, col) =>
            grid(row - 1).update(col, "\u265B")
          }
          grid 
        case false => notSolvableFunction
      } 
    case _ => Queens8IsValid(move, grid) match
      case false => invalid1PlayerFunction
      case true => Queens8UpdateGrid(move.charAt(0).asDigit - 1, map(move.charAt(1)), grid)
  }
}