import ch.hevs.gdx2d.components.bitmaps.BitmapImage
import ch.hevs.gdx2d.lib.GdxGraphics
import ch.hevs.gdx2d.lib.interfaces.DrawableObject
import com.badlogic.gdx.math.Vector2

class Explosion extends DrawableObject {
  private val SPRITE_WIDTH: Int = 16
  private val SPRITE_HEIGHT: Int = 16
  private val explosionBitmap: BitmapImage = new BitmapImage("data/Res/Characters/Explosion.png")
  private val position: Vector2 = new Vector2(0,0)

  var show: Boolean = false

  def setPostion(posX: Float,posY: Float): Unit = {
    position.x = posX
    position.y = posY
  }

  override def draw(g: GdxGraphics): Unit = {
    if (show) {
      g.drawTransformedPicture(position.x, position.y, 90f, SPRITE_WIDTH, SPRITE_HEIGHT, explosionBitmap)
    }
  }
}
