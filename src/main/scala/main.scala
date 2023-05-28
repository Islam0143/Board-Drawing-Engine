package Games

import java.awt.*
import javax.swing.*
import scala.collection.immutable.List

@main def main(): Unit = {
  val frame = new JFrame("Board Drawing Game")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(500, 500)
  val panel = new JPanel(new GridLayout(6, 1))
  Range(1, 7)
    .map(i => new JButton(i + ". " + gamesName(i)))
    .map(button => {
      button.setFont(new Font("Arial", Font.BOLD, 16))
      button.setBackground(Color.WHITE)
      panel.add(button)
    })
  panel.setPreferredSize(frame.getSize)
  frame.add(panel)
  frame.setVisible(true)
  gameChooser(frame)
}