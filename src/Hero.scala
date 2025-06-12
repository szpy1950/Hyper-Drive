import ch.hevs.gdx2d.components.bitmaps.BitmapImage
import ch.hevs.gdx2d.lib.GdxGraphics
import ch.hevs.gdx2d.lib.interfaces.DrawableObject
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.math.{Rectangle, Vector2}

class Hero extends DrawableObject {
  /*
  Section: Variables
   */

  // Bookmark: Map information
  private val windowWidth = 20 * 32

  // Bookmark: Image settings
  private val SPRITE_WIDTH = 16
  private val SPRITE_HEIGHT = 16
  private val carBitmap: BitmapImage = new BitmapImage("data/Res/Characters/BlackOut.png")

  // Bookmark: Position settings
  private val initialPosition: Vector2 = new Vector2(335,100)
  private var position: Vector2 = initialPosition
  private var displacement: Float = 0

  // Bookmark: Rectangle
  private val rectangle: Rectangle = new Rectangle()

  var show: Boolean = true

  /*
  Section: Methods
   */

  def getPosition: Vector2 = position

  def setPosition(newPos: Vector2): Unit = {
    position = newPos
  }

  def getRectangle: Rectangle = {
    rectangle
  }

  def setRectangle(position: Vector2, width: Int, height: Int): Unit = {
    rectangle.set(position.x - 8,position.y - 8,width,height)
  }

  def setDisplacement(distance: Float): Unit = {
    displacement += distance
  }

  def getDisplacement:Float ={
    displacement
  }

  def moveUp(speed: Float): Unit = {
    position.add(0, speed)
  }

  def go(direction: String): Unit = {
    direction match {
      case "RIGHT" => position.add(2f, 0)
      case "LEFT" => position.add(-2f, 0)
      case "UP" => position.add(0, displacement)
      case _ => position.add(0, 0)
    }
    displacement = 0 // Reset displacement after use
  }

  override def draw(g: GdxGraphics): Unit = {
    if (show) {
      g.drawTransformedPicture(position.x, position.y, 90f, SPRITE_HEIGHT, SPRITE_WIDTH, carBitmap)
    }
  }
}
