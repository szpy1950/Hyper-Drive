package Testing

import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import com.badlogic.gdx.Input
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer.Cell
import com.badlogic.gdx.maps.tiled.{TiledMap, TiledMapTileLayer, TiledMapTileSet, TmxMapLoader, renderers}
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.maps.tiled.tiles.StaticTiledMapTile
import com.badlogic.gdx.math.Vector2

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Test extends PortableApplication (20 * 32, 21 * 32) {
  var tiledMap: TiledMap = null
  var tiledMapRenderer: OrthogonalTiledMapRenderer = null
  val zoom = 0.5f
  var tiledSet: TiledMapTileSet = null
  var cameraPosition: Vector2 = new Vector2(0,0)
  var enemies: ArrayBuffer[Enemy] = null

  override def onInit(): Unit = {

    setTitle("Traffic Rider")
    tiledMap = new TmxMapLoader().load("data/Tiled/highway.tmx")
    tiledMapRenderer = new OrthogonalTiledMapRenderer(tiledMap)
    tiledSet = tiledMap.getTileSets.getTileSet("TopDownTileset")
    enemies = new ArrayBuffer[Enemy]()

//    tiledMap.getLayers.get("Tile Layer 1").asInstanceOf[TiledMapTileLayer].setCell(0, 0, null)
  }
  override def onGraphicRender(g: GdxGraphics): Unit = {
    g.clear()
    g.zoom(zoom)
    g.moveCamera(cameraPosition.x,cameraPosition.y)
    generate()

    for (i <- enemies) {
      i.draw(g)
    }

    tiledMapRenderer.setView(g.getCamera)
    tiledMapRenderer.render()

    g.drawFPS()
  }

  def generate(): Unit = {
    if (cameraPosition.y + getWindowHeight / 2 >= tiledMap.getLayers.get("Tile Layer 1").asInstanceOf[TiledMapTileLayer].getHeight * tiledMap.getLayers.get("Tile Layer 1").asInstanceOf[TiledMapTileLayer].getTileHeight) {
      println("hello")
      var layer: TiledMapTileLayer = tiledMap.getLayers.get("Tile Layer 1").asInstanceOf[TiledMapTileLayer]
      var layer1: TiledMapTileLayer = tiledMap.getLayers.get("Tile Layer 2").asInstanceOf[TiledMapTileLayer]
      println(layer1)

      // Bookmark: Rebuild old layer into new
      val newLayer = new TiledMapTileLayer(layer.getWidth, layer.getHeight + 1, 32, 32) // Adds one more row to map
      val newLayer1 = new TiledMapTileLayer(layer1.getWidth, layer1.getHeight + 1, 32, 32)
      for (x <- 0 until layer.getWidth; y <- 0 until layer.getHeight) { // Retrieves all previous map information
        newLayer.setCell(x, y, layer.getCell(x, y))
        newLayer1.setCell(x, y, layer1.getCell(x, y))
      }

      // Bookmark: Remove unneeded cells
      for (x <- 0 until layer.getWidth; y <- 0 until cameraPosition.y.toInt - 1000) {
        newLayer.setCell(x,y,null)
      }

      // Bookmark: Prepare tile type and position to add
      val newTile: StaticTiledMapTile = new StaticTiledMapTile(tiledSet.getTile(417).getTextureRegion)

      val cell: Cell = new Cell
      cell.setTile(newTile)
      for (x <- 0 to newLayer.getWidth) {
        newLayer.setCell(x, newLayer.getHeight - 1, cell)
      }


      // Bookmark: Update layer
      newLayer.setName("Tile Layer 1")
      newLayer1.setName("Tile Layer 2")
      tiledMap.getLayers.remove(layer)
      tiledMap.getLayers.remove(layer1)
      tiledMap.getLayers.add(newLayer)
      tiledMap.getLayers.add(newLayer1)
      tiledMapRenderer = new OrthogonalTiledMapRenderer(tiledMap)
    }
  }

  override def onKeyDown(keycode: Int): Unit = {
    super.onKeyDown(keycode)
    keycode match {
      case Input.Keys.W => cameraPosition.add(0,32)
      case Input.Keys.D => cameraPosition.add(32,0)
      case Input.Keys.A => cameraPosition.add(-32,0)
      case Input.Keys.S => cameraPosition.add(0,-32)
      case Input.Keys.SPACE => SpawnEnemies()
      case _ =>
    }
  }

  private def SpawnEnemies(): Unit = {
    for (i <- 0 to Random.nextInt(5)) {
      enemies += new Enemy
    }
  }
}

object Test {
  def main (args: Array[String]):Unit = new Test
}