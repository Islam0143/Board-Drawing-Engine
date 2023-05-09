package Games
import java.awt._

def createCircleIcon(size: Int, color: Color): java.awt.Image = {
  val image = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
  val g = image.getGraphics.asInstanceOf[Graphics2D]
  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g.setColor(color)
  g.fillOval(0, 0, size, size)
  g.dispose()
  image
}

def map(ch: Char): Int = "abcdefghi".toList.indexOf(ch)