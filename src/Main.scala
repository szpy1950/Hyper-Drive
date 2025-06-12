import ch.hevs.gdx2d.components.bitmaps.BitmapImage
import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import ch.hevs.gdx2d.lib.utils.Logger
import com.badlogic.gdx.{Gdx, Input}
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer.Cell
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.maps.tiled.tiles.StaticTiledMapTile
import com.badlogic.gdx.maps.tiled.{TiledMap, TiledMapRenderer, TiledMapTileLayer, TiledMapTileSet, TmxMapLoader, tiles}
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.graphics.Color

import java.util
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    new Main
  }
}

class Main extends PortableApplication(20 * 32, 21 * 32) {
  /*
  Section: Variable list
   */

  // Bookmark: Tiled map managers
  private var tiledMap: TiledMap = null
  private var tiledMapRenderer: TiledMapRenderer = null
  private var natureTiledSet: TiledMapTileSet = null
  private var roadTiledSet: TiledMapTileSet = null
  private var tileLayer: TiledMapTileLayer = null
  private var tileSwitch: Boolean = false

  // Bookmark: Camera manipulation
  private var zoom: Float = 0
  private var cameraPosition = new Vector2(0, 0)

  // Bookmark: Sprite settings
  private var hero: Hero = null
  private var enemies: ArrayBuffer[Enemy] = null
  private var explosion: Explosion = null

  // Bookmark: Key management
  private val keyStatus: util.Map[Integer, Boolean] = new util.TreeMap[Integer, Boolean]

  // Bookmark: Gameplay booleans
  private var isGameOver: Boolean = false
  private var mainGame: Boolean = false
  private var startScreen: Boolean = true

  // Bookmark: Timer
  var timer: Float = 0
  var deltaY: Int = 0

  // Bookmark: Score system
  private var score: Float = 0f
  private var lastPosition: Float = 0f

  // Bookmark: Speed gauge system
  private var currentSpeed: Float = 1.0f
  private var minSpeed: Float = 2f
  private val maxSpeed: Float = 8.0f
  private val speedIncrement: Float = 0.1f

  /*
  Section: Initialization
   */

  override def onInit(): Unit = {
    deltaY = 0
    timer = 0
    zoom = 0.5f
    currentSpeed = minSpeed
    score = 0f

    // Bookmark: Create characters
    hero = new Hero
    lastPosition = hero.getPosition.y

    //    enemy = new Enemy
    enemies = ArrayBuffer[Enemy]()
    explosion = new Explosion

    // Bookmark: Create map
    setTitle("Traffic Rider")
    tiledMap = new TmxMapLoader().load("data/Tiled/highway.tmx")
    tiledMapRenderer = new OrthogonalTiledMapRenderer(tiledMap)
    natureTiledSet = tiledMap.getTileSets.getTileSet("TopDownTileset")
    roadTiledSet = tiledMap.getTileSets.getTileSet("City Prop Tileset update 2")

    // Bookmark: Initialize keys
    keyStatus.put(Input.Keys.A, false) // right
    keyStatus.put(Input.Keys.D, false) // left
    keyStatus.put(Input.Keys.UP, false) // speed up
    keyStatus.put(Input.Keys.DOWN, false) // speed down

    // Bookmark: Layer references
    tileLayer = tiledMap.getLayers.get("Tile Layer 1").asInstanceOf[TiledMapTileLayer]

    // Bookmark: Set game over to false
    isGameOver = false

    Logger.log("New session")
  }

  /*
  Section: Execution
   */



  override def onGraphicRender(g: GdxGraphics): Unit = {
    // Bookmark: cleanup
    g.clear()

    if (startScreen) {
      start()
      val bitMap = new BitmapImage("data/Res/loadingScreen.jpeg")
      g.drawBackground(bitMap,0f,0f)
    }

    if (mainGame) {
      timer += Gdx.graphics.getDeltaTime

      // Bookmark: Character managers
      if (!isGameOver) {
        manageSpeed()
        updateScore()
        spawnEnemies()
        manageHero()
        manageEnemy()
        generate()
        g.moveCamera(hero.getPosition.x - 160, hero.getPosition.y - 20)
        tiledMapRenderer.setView(g.getCamera)
      }
      checkCollision()
      tiledMapRenderer.render()
      // Bookmark: Camera management
      g.zoom(zoom)

      // Bookmark: Drawing all graphical elements
      hero.draw(g)
      for (enemy <- enemies) {
        enemy.draw(g)
      }
      explosion.draw(g)
      drawSpeedGauge(g)
      drawScore(g)
      if (isGameOver) {
        g.drawString(hero.getPosition.x - 48, hero.getPosition.y + 100, "GAME OVER")
      }
//      g.drawSchoolLogo()
      g.drawFPS()
    }
  }

  /*
  Section: Score management
   */

  def updateScore(): Unit = {
    val currentPosition = hero.getPosition.y
    val distanceTraveled = currentPosition - lastPosition

    if (distanceTraveled > 0) {
      val pointsPerSecond = currentSpeed // 1 point per second at speed = 1
      val pointsGained = (pointsPerSecond *( 1f/ 60f)) * 0.5f
      score += pointsGained
    }

    val milestone = (score / 10).toInt
    if (milestone > (lastPosition / 10).toInt) {
      minSpeed += 0.5f
      if (minSpeed >= 6f) minSpeed = 6f
      if (currentSpeed < minSpeed) currentSpeed = minSpeed
      println(s"Min speed increased to: $minSpeed")
    }



    lastPosition = currentPosition
  }


  def drawScore(g: GdxGraphics): Unit = {
    g.setColor(Color.WHITE)

    // Draw score at top left, relative to camera position
    val scoreX = hero.getPosition.x - 150  // Adjust position relative to hero
    val scoreY = hero.getPosition.y + 300  // Adjust position relative to hero

    val scoreDisplay = score.toInt
    g.drawString(scoreX, scoreY, s"Score: $scoreDisplay")
  }

  /*
  Section: Character management
   */

  def manageHero(): Unit = {
    var goalDirection: String = ""
    var nextPos: Float = 0f

    // Autodrive with current speed
    hero.moveUp(currentSpeed)

    if (keyStatus.get(Input.Keys.D) || keyStatus.get(Input.Keys.RIGHT)) {
      goalDirection = "RIGHT"
      nextPos = hero.getPosition.x + 1.8f
    }
    if (keyStatus.get(Input.Keys.A) || keyStatus.get(Input.Keys.LEFT)) {
      goalDirection = "LEFT"
      nextPos = hero.getPosition.x - 1.8f
    }
    if (isDrivable(nextPos)) {
      hero.go(goalDirection)
    }
  }

  def manageSpeed(): Unit = {
    if (keyStatus.get(Input.Keys.W)) {
      if (currentSpeed < maxSpeed) {
        currentSpeed += speedIncrement
        if (currentSpeed > maxSpeed) currentSpeed = maxSpeed
        println(s"Speed increased to: $currentSpeed")
      }
    }
    if (keyStatus.get(Input.Keys.S)) {
      if (currentSpeed > minSpeed) {
        currentSpeed -= speedIncrement
        if (currentSpeed < minSpeed) currentSpeed = minSpeed
        println(s"Speed decreased to: $currentSpeed")
      }
    }
  }

  def drawSpeedGauge(g: GdxGraphics): Unit = {
    val gaugeX = hero.getPosition.x + 120
    val gaugeY = hero.getPosition.y + 80
    val gaugeWidth = 20
    val gaugeHeight = 200
    val speedRatio = currentSpeed / maxSpeed
    val fillHeight = (gaugeHeight * speedRatio).toInt

    // Calculate minimum speed line position
    val minSpeedRatio = minSpeed / maxSpeed
    val minSpeedLineY = gaugeY + (gaugeHeight * minSpeedRatio).toInt

    // Create colors manually
    val darkGray = new com.badlogic.gdx.graphics.Color(0.25f, 0.25f, 0.25f, 1.0f)
    val white = new com.badlogic.gdx.graphics.Color(1.0f, 1.0f, 1.0f, 1.0f)
    val red = new com.badlogic.gdx.graphics.Color(1.0f, 0.0f, 0.0f, 1.0f)
    val yellow = new com.badlogic.gdx.graphics.Color(1.0f, 1.0f, 0.0f, 1.0f)
    val green = new com.badlogic.gdx.graphics.Color(0.0f, 1.0f, 0.0f, 1.0f)

    // Draw gauge background
    g.setColor(darkGray)
    for (i <- 0 until gaugeHeight by 2) {
      g.drawLine(gaugeX, gaugeY + i, gaugeX + gaugeWidth, gaugeY + i)
    }

    // Draw speed fill
    if (speedRatio > 0.8f) {
      g.setColor(red)
    } else if (speedRatio > 0.5f) {
      g.setColor(yellow)
    } else {
      g.setColor(green)
    }

    // Fill the gauge
    for (i <- 0 until fillHeight by 2) {
      g.drawLine(gaugeX + 1, gaugeY + i, gaugeX + gaugeWidth - 1, gaugeY + i)
    }

    // Draw gauge border using lines
    g.setColor(white)
    g.drawLine(gaugeX, gaugeY, gaugeX + gaugeWidth, gaugeY) // bottom
    g.drawLine(gaugeX, gaugeY + gaugeHeight, gaugeX + gaugeWidth, gaugeY + gaugeHeight) // top
    g.drawLine(gaugeX, gaugeY, gaugeX, gaugeY + gaugeHeight) // left
    g.drawLine(gaugeX + gaugeWidth, gaugeY, gaugeX + gaugeWidth, gaugeY + gaugeHeight) // right

    // Draw minimum speed line (white line)
    g.setColor(white)
    g.drawLine(gaugeX - 2, minSpeedLineY, gaugeX + gaugeWidth + 2, minSpeedLineY) // Line extends slightly beyond gauge

    // Draw only "Speed" text at the top
    g.setColor(white)
    g.drawString(gaugeX - 10, gaugeY + gaugeHeight + 20, "Speed")
  }

  def spawnEnemies(): Unit ={
    if (timer.toInt == 1) {
      for (i <- 0 to Random.nextInt(3)) {
        enemies += new Enemy(hero.getPosition.y,deltaY)
      }
      timer -= 1
      deltaY += 75
    }
  }

  def manageEnemy(): Unit ={
    for (enemy <- enemies) {
      enemy.drive()
    }
  }

  def checkCollision(): Unit ={
    hero.setRectangle(hero.getPosition,16,16 + 15)
    for (enemy <- enemies){
      enemy.setRectangle(enemy.getPosition,16,16 + 15)

      if (hero.getRectangle.overlaps(enemy.getRectangle)) {
        gameover()
      }
    }
  }

  def isDrivable(nextPos: Float): Boolean = {
    if (nextPos >= getWindowWidth / 2 + 2 * 32 - 8 || (nextPos <= getWindowWidth / 2 - 64 + 8)) {
      false
    }
    else true
  }

  override def onKeyUp(keycode: Int): Unit = {
    super.onKeyUp(keycode)
    keyStatus.put(keycode, false)
  }

  override def onKeyDown(keycode: Int): Unit = {
    super.onKeyDown(keycode)
    keyStatus.put(keycode, true)
  }

  /*
 Section: Gameplay events
  */

  def gameover(): Unit = {
    explosion.setPostion(hero.getPosition.x,hero.getPosition.y + 16)
    explosion.show = true
    isGameOver = true

    if (keyStatus.get(Input.Keys.SPACE)) {
      onInit()
    }
  }

  def start(): Unit = {
    if (keyStatus.get(Input.Keys.SPACE)) {
      startScreen = false
      mainGame = true
    }
  }

  /*
  Section: Procedural generation
   */

  def generate(): Unit = {
    if (hero.getPosition.y + getWindowHeight / 2 >= tileLayer.getHeight * 32) {
      var layer: TiledMapTileLayer = tiledMap.getLayers.get("Tile Layer 1").asInstanceOf[TiledMapTileLayer]
      var layer1: TiledMapTileLayer = tiledMap.getLayers.get("Tile Layer 2").asInstanceOf[TiledMapTileLayer]

      // Bookmark: Rebuild old layer into new
      val newLayer = new TiledMapTileLayer(layer.getWidth, layer.getHeight + 1, 32, 32) // Adds one more row to map
      val newLayer1 = new TiledMapTileLayer(layer1.getWidth, layer1.getHeight + 1, 32, 32)
      for (x <- 0 until layer.getWidth; y <- 0 until layer.getHeight) { // Retrieves all previous map information
        newLayer.setCell(x, y, layer.getCell(x, y))
        newLayer1.setCell(x, y, layer1.getCell(x, y))
      }

      // Bookmark: Delete unnecessary tiles
      for (x <- 0 until layer.getWidth by 32; y <- 0 until hero.getPosition.y.toInt - 64 by 32) {
        newLayer.setCell(x, y, null)
      }

      // Bookmark: Prepare tile type and position to add
      val grassTile: StaticTiledMapTile = new StaticTiledMapTile(natureTiledSet.getTile(417).getTextureRegion)
      val roadTile1: StaticTiledMapTile = new StaticTiledMapTile(roadTiledSet.getTile(51).getTextureRegion)
      val roadTile2: StaticTiledMapTile = new StaticTiledMapTile(roadTiledSet.getTile(52).getTextureRegion)
      var cell: Cell = null

      for (x <- 0 to newLayer.getWidth) {
        if (x == 8 || x == 10) {
          cell = new Cell
          cell.setTile(roadTile1)
          newLayer.setCell(x, newLayer.getHeight - 1, cell)
        }
        else if (x == 9 || x == 11) {
          cell = new Cell
          cell.setTile(roadTile2)
          newLayer.setCell(x, newLayer.getHeight - 1,cell)
        }
        else {
          cell = new Cell
          cell.setTile(grassTile)
          newLayer.setCell(x, newLayer.getHeight - 1, cell)
        }
        vegetationGenerator(newLayer1)
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

  def vegetationGenerator(layer: TiledMapTileLayer): Unit = {
    val shrubTile: StaticTiledMapTile = new StaticTiledMapTile(natureTiledSet.getTile(368).getTextureRegion)
    var cell: Cell = null
    val trees: Array[Int] = Array(334, 335, 320, 321)
    var vegeTile: StaticTiledMapTile = null

    for (x <- 0 to layer.getWidth) {
      if (x == 7 || x == 12) {
        // Always place shrubs at road borders
        cell = new Cell
        cell.setTile(shrubTile)
        layer.setCell(x, layer.getHeight - 1, cell)
      }
      else if (x == 6 || x == 13) {
        // Random chance to place trees on the sides
        if (Random.nextFloat() < 0.7f) { // 70% chance to place a tree
          val ID = trees(Random.nextInt(trees.length))
          cell = new Cell
          vegeTile = new StaticTiledMapTile(natureTiledSet.getTile(ID).getTextureRegion)
          cell.setTile(vegeTile)
          layer.setCell(x, layer.getHeight - 1, cell)
        } else {
          layer.setCell(x, layer.getHeight - 1, null)
        }
      }
      else if (x < 6 || x > 13) {
        // Random vegetation in far areas
        if (Random.nextFloat() < 0.3f) { // 30% chance for sparse vegetation
          if (Random.nextBoolean()) {
            // Place shrub
            cell = new Cell
            cell.setTile(shrubTile)
            layer.setCell(x, layer.getHeight - 1, cell)
          } else {
            // Place random tree
            val ID = trees(Random.nextInt(trees.length))
            cell = new Cell
            vegeTile = new StaticTiledMapTile(natureTiledSet.getTile(ID).getTextureRegion)
            cell.setTile(vegeTile)
            layer.setCell(x, layer.getHeight - 1, cell)
          }
        } else {
          layer.setCell(x, layer.getHeight - 1, null)
        }
      }
      else {
        layer.setCell(x, layer.getHeight - 1, null)
      }
    }
  }


  /*
  Section: Finishing off
   */

  override def onDispose(): Unit = {
    super.onDispose()
    Logger.log("Bye bye")
  }
}