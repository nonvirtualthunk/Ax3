
package arx.rog2.game.application

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.datastructures.FiniteGrid2D
import arx.core.datastructures.MultiMap
import arx.core.math.Recti
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.core.vec.Vec3f
import arx.core.vec.Vec4i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.EngineCore
import arx.engine.advanced.Engine
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CanvasGraphicsComponent
import arx.engine.graphics.data.PovData
import arx.engine.graphics.data.TGraphicsData
import arx.engine.simple.Canvas
import arx.graphics.Image
import arx.graphics.images.Palette
import arx.graphics.images.PaletteGroup
import arx.graphics.images.PaletteImage
import arx.graphics.pov.TopDownCamera
import arx.resource.ResourceManager
import arx.rog2.game.application.DungeonMapGenerator.makeEven
import arx.rog2.game.application.DungeonMapGenerator.makeOdd
import arx.rog2.game.application.RogMazeGeneratorApplication.graphicsEngine
import arx.rog2.game.application.RogMazeGeneratorApplication.graphicsWorld
import arx.rog2.game.application.RogMazeGeneratorApplication.graphicsWorld
import org.lwjgl.glfw.GLFW
import org.lwjgl.opengl.GL11

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._


class DungeonMap(val dim: ReadVec2i) {
	val grid = FiniteGrid2D[Boolean](dim, false)
	val regions = FiniteGrid2D[Int](dim, -1)
	var currentRegion = -1
	var passageRegion = 0
	var scale = 1
	var roomsByRegion = Map[Int, DungeonRoom]()

	def toVC(x : Int, y : Int) : ReadVec2i = {
		Vec2i(VoxelCoord.Center.x + (x - dim.x/2) * scale, VoxelCoord.Center.y + (y - dim.y/2) * scale)
	}

	def carve(v: ReadVec2i): Unit = {
		carve(v.x, v.y)
	}

	def carve(x: Int, y: Int): Unit = {
		grid(x, y) = true
		regions(x, y) = currentRegion
	}

	def newRegion(): Unit = {
		currentRegion += 1
	}
}

case class DungeonRoom(rect : Recti, regionIndex : Int) {

}

class DungeonMapGenerator {

	case class Cell(pos: ReadVec2i)

	val c2d = Array(Vec2i(-1, 0), Vec2i(1, 0), Vec2i(0, 1), Vec2i(0, -1))

	var windingPercent = 30
	var roomSize = 30
	var additionalJunctionPcnt = 3.5f
	var roomCount = 200
	var mazeCellSelector : (Int) => Int = i => rand(0,i)

	def generateDungeon(dims: ReadVec2i) = {
		val dungeon = new DungeonMap(dims)
		generateRooms(dungeon)
		generateMaze(dungeon)
		connectRegions(dungeon)
		removeDeadEnds(dungeon)
		dungeon
	}

	def generateRooms(dungeon: DungeonMap) = {
		val dim = dungeon.dim
		for (i <- 0 until roomCount) {
			val rect = if (i == 0) {
				Recti(dim.x/2-5,dim.y/2-5,10,10)
			} else {
				Recti(makeOdd(rand(1, dim.x - roomSize)), makeOdd(rand(1, dim.y - roomSize)), makeEven(rand(3, roomSize)), makeEven(rand(3, roomSize)))
			}
			var collide = false
			for (x <- rect.minX to rect.maxX; y <- rect.minY to rect.maxY) {
				if (dungeon.grid(x, y)) {
					collide = true
				}
			}
			if (!collide) {
				dungeon.newRegion()
				dungeon.roomsByRegion += dungeon.currentRegion -> DungeonRoom(rect, dungeon.currentRegion)
				for (x <- rect.minX to rect.maxX; y <- rect.minY to rect.maxY) {
					dungeon.carve(x, y)
				}
			}
		}
	}

	def generateMaze(dungeon: DungeonMap) = {
		dungeon.newRegion()
		val grid = dungeon.grid
		dungeon.passageRegion = dungeon.currentRegion

		def inBounds(x: Int, y: Int) = x >= 1 && y >= 1 && x < grid.dimensions.x - 1 && y < grid.dimensions.y - 1


		val cells = new mutable.ArrayBuffer[Cell]()
		var startPos: ReadVec2i = null
		while (startPos == null || grid(startPos)) {
			startPos = ReadVec2i(rand(1, grid.dimensions.x / 2) * 2 - 1, rand(1, grid.dimensions.y / 2) * 2 - 1)
		}
		dungeon.carve(startPos.x, startPos.y)
		cells.append(Cell(startPos))

		var lastDir = -1
		while (cells.nonEmpty) {
			val nextIdx = mazeCellSelector(cells.size)
			val cell = cells(nextIdx)

			var possibles = List[Int]()
			for (q <- 0 until 4) {
				val dx = c2d(q).x
				val dy = c2d(q).y

				val x = cell.pos.x + dx
				val y = cell.pos.y + dy
				if (inBounds(x + dx, y + dy) && !grid(x, y) && !grid(x + dx, y + dy)) {
					possibles ::= q
				}
			}

			if (possibles.nonEmpty) {
				val chosen = if (possibles.contains(lastDir) && rand(0, 100) > windingPercent) {
					lastDir
				} else {
					randFrom(possibles)
				}

				cells.append(Cell(cell.pos + c2d(chosen) * 2))
				dungeon.carve(cell.pos + c2d(chosen))
				dungeon.carve(cell.pos + c2d(chosen) * 2)
				lastDir = chosen
			} else {
				cells(nextIdx) = cells.last
				cells.remove(cells.size - 1)
				lastDir = -1
			}
		}

		grid
	}

	def addJunction(dungeon: DungeonMap, v: ReadVec2i): Unit = {
		dungeon.carve(v)
	}

	def connectRegions(dungeon: DungeonMap): Unit = {
		val connectionMap = new MultiMap[ReadVec2i, Int]

		for (x <- 1 until dungeon.dim.x - 1 optimized; y <- 1 until dungeon.dim.y - 1 optimized) {
			if (!dungeon.grid(x, y)) {
				var regions = Set[Int]()
				for (q <- 0 until 4 optimized) {
					val r = dungeon.regions(x + c2d(q).x, y + c2d(q).y)
					if (r != -1) {
						regions += r
					}
				}
				if (regions.size >= 2) {
					connectionMap.addAll(Vec2i(x, y), regions)
				}
			}
		}

		var connectors = connectionMap.intern.keys.toList
		val merged = new mutable.HashMap[Int, Int]()
		val openRegions = new mutable.HashSet[Int]()
		for (i <- 0 to dungeon.currentRegion) {
			merged.put(i, i)
			openRegions.add(i)
		}

		while (openRegions.size > 1) {
			val pos = randFrom(connectors)

			addJunction(dungeon, pos)

			val regions = connectionMap.get(pos).map(r => merged(r))
			val dest = regions.head
			val sources = regions.tail

			for (i <- 0 to dungeon.currentRegion) {
				if (sources.contains(merged(i))) {
					merged(i) = dest
				}
			}

			sources.foreach(s => openRegions.remove(s))

			connectors = connectors.filterNot(c => {
				if ((c.x - pos.x).abs < 2 && (c.y - pos.y).abs < 2) {
					true
				} else {
					if (connectionMap.get(c).map(r => merged(r)).toSet.size > 1) {
						false
					} else {

						if (rand(0, 1000) < (additionalJunctionPcnt * 10).toInt) {
							addJunction(dungeon, c)
						}
						true
					}
				}
			})
		}
	}

	def removeDeadEnds(dungeon: DungeonMap): Unit = {
		var done = false
		while (!done) {
			done = true

			for (x <- 1 until dungeon.dim.x - 1 optimized; y <- 1 until dungeon.dim.y - 1 optimized) {
				if (dungeon.grid(x, y)) {
					var exits = 0
					for (q <- 0 until 4 optimized) {
						if (dungeon.grid(x + c2d(q).x, y + c2d(q).y)) {
							exits += 1
						}
					}
					if (exits == 1) {
						done = false
						dungeon.grid(x, y) = false
					}
				}
			}
		}
	}
}

object DungeonMapGenerator {
	def makeOdd(i: Int) = (i / 2) * 2 + 1

	def makeEven(i: Int) = (i / 2) * 2
}

abstract class ImageDisplayEngine extends Engine {
	EngineCore.windowWidth = 1000
	EngineCore.windowHeight = 1000

	def generateImages : List[Image]

	override def setUpEngine(): Unit = {
		graphicsWorld[ImageData].images = generateImages

		graphicsEngine.addComponent[ImageDisplayGraphicsComponent]

		graphicsWorld[PovData].pov = new TopDownCamera(15.0f)

		controlEngine.onEvent {
			case KeyPressEvent(key, _, _) if key == GLFW.GLFW_KEY_S => Image.save(graphicsWorld[ImageData].images.head, "/tmp/image.png")
		}
	}
}

object RogMazeGeneratorApplication extends ImageDisplayEngine {
	override def generateImages = {

		val dim = Vec2i(200, 200)

		val gen = new DungeonMapGenerator()
		gen.mazeCellSelector = (size) => rand((size-20).max(0),size)
		gen.windingPercent = 15
		gen.roomCount = 300
		val dungeon = gen.generateDungeon(dim)

		val image = Image.withDimensions(dim.x, dim.y, Vec4i(0, 0, 0, 255))
		val colors = fillArray(dungeon.currentRegion + 1)((i) => Vec4i(rand(50, 255), rand(50, 255), rand(50, 255), 255))
		for (x <- 0 until dim.x; y <- 0 until dim.y) {
			if (dungeon.grid(x, y)) {
				image(x, y) = colors(dungeon.regions(x, y))
			} else if (x % (dim.x - 1) == 0 || y % (dim.y - 1) == 0) {
				image(x, y) = Vec4i(255, 0, 0, 255)
			}
		}
		List(image)
	}
}

class ImageData extends TGraphicsData {
	var images: List[Image] = List()
}

class ImageDisplayGraphicsComponent(ge: GraphicsEngine) extends CanvasGraphicsComponent(ge) {
	canvas.useTexFilters(GL11.GL_NEAREST, GL11.GL_NEAREST)

	override def draw(canvas: Canvas): Unit = {
		val images = graphics[ImageData].images
		val imgSize = 10.0f / images.size
		val offset = -imgSize * (images.size-1) * 0.5f
		for ((img,index) <- images.zipWithIndex) {
			canvas.quad(Vec3f(offset + index * imgSize, 0.0f,0.0f))
				.withDimensions(imgSize,imgSize)
				.withTexture(img)
				.draw()
		}
	}
}