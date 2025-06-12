package Testing

import ch.hevs.gdx2d.components.bitmaps.BitmapImage
import ch.hevs.gdx2d.lib.GdxGraphics
import ch.hevs.gdx2d.lib.interfaces.DrawableObject
import com.badlogic.gdx.math.{Rectangle, Vector2}

import scala.util.Random

class Enemy extends DrawableObject {
  /*
  Section: Variables
   */

  // Bookmark: Map information
  private val windowWidth: Int = 20 * 32
  private val windowHeight: Int = 21 * 32

  // Bookmark: Sprite graphical design
  private val SPRITE_WIDTH = 16
  private val SPRITE_HEIGHT = 16
  private val skins: Array[String] = Array("BlueStrip","GreenStrip","PinkStrip","RedStrip","WhiteStrip")
  private val skin: String = s"data/Res/Characters/${skins(Random.nextInt(skins.length))}.png"
  private val carBitmap: BitmapImage = new BitmapImage(skin)

  // Bookmark: Position settings
  private val initX: Array[Int] = Array(335, 335 + 32, 335 - 32, 335 - 64)
  private val initialPosition: Vector2 = new Vector2(initX(Random.nextInt(initX.length)), (windowHeight * 0.5 + 3 * SPRITE_HEIGHT).toInt)
  private var orientation: Float = 0
  if (initialPosition.x < 335) {
    orientation = 270f
  }
  else orientation = 90f
  private var position: Vector2 = initialPosition

  // Bookmark: Rectangle
  private val rectangle = new Rectangle()

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

  def drive(): Unit = {
    if (orientation == 90f) {
      position.add(0,1f)
    }
    else {
      position.add(0,-1.5f)
    }
  }

  override def draw(g: GdxGraphics): Unit = {
    if (show) {
      g.drawTransformedPicture(position.x, position.y, orientation, SPRITE_HEIGHT, SPRITE_WIDTH, carBitmap)
    }
  }
}
