package arx.axistential.testbed.thermo.application

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/26/13
 * Time: 1:21 PM
 */

import arx.Prelude._
import arx.axistential.game.components.Cloud
import arx.axistential.game.components.WeatherComponent
import arx.axistential.game.data.world.ScaleData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.data.world.WindData
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.meteorological.WindWorldGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.gen.ArxGenerators._
import arx.core.gen.SimplexNoise
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.CellGrid2D
import arx.graphics.Image

import scalaxy.loops._


class VectorFieldTestbed extends DeadSimpleConflux {
	def dims = Vec3i(512,512,512)
	val heightmapImage = Image.withDimensions(dims.x,dims.y)

	private val denom = 2
	val vectorLength = CellGrid2D[Float](denom)
//	val heightmapTexture = Texture.fromImage( heightmapImage )

	val vecImage = image("test/vectorArrow.png")

	val numCloudPoints = 20
	var cloudPoints = List[ReadVec2f]()
	var cloudConnections = List[List[Int]]()
	var equilibriumDistances = List[List[Float]]()
	val cloudSpeed = 0.02f
	val desiredGap = 15

	val SN = new SimplexNoise(System.currentTimeMillis())
	val dyngen = Scale(0.01f) >> Fractal(3)(Simplex(SN)) >> Mult(math.Pi.toFloat * 0.35f)

	def world = gameEngine.activeWorld

	override def setUpGameEngine(): Unit = {
		gameEngine.environments ::= new World

		val WG = new WorldGenerator
		val wgDesc = new WorldGenerationDescription(
			new TopologyGenerator ::
				new GeologicalFeatureGenerator ::
				new WindWorldGenerator ::
				Nil,
			dims
		)

		WG.generateWorld(world,wgDesc)

		gameEngine.environments ::= world

		gameEngine.addComponent(new WeatherComponent)
	}

	override def init(): Unit = {
		super.init ()

		val topological = world.aux[TopologicalData]
		val scaleTo = world.aux[ScaleData].MountainPeak.inVoxels
		val reg = world.worldRegionAsSpatialRegion

		for ( x <- 0 until dims.x optimized ; y <- 0 until dims.y optimized ) {
			val h = topological.heightmap(reg.minX + x,reg.minY + y)
			val d = topological.depthmap(reg.minX + x,reg.minY + y)

//			val outlier = h > (mean + stdDev * 2.5)
			val rgb = if ( h > d ) { ((h / scaleTo + 0.1f).clamp(0.0f,1.0f) * 255).toInt } else { 0 }
//			val color = if ( outlier ) { Vec4i(rgb,0,0,255) } else { Vec4i(rgb,255) }
			val color = Vec4i(rgb,255)
			heightmapImage(x,y) = color
		}

//		val cloudGen = Scale(0.085f) >> Fractal(3)(Simplex(new SimplexNoise(System.currentTimeMillis()))) >> Mult(20)
//		for ( i <- 0 until numCloudPoints ) {
//			cloudPoints ::= Vec2f( reg.minX + cloudGen(i) , (reg.minY + reg.dimensions.y * (0.25f + 0.5f * (i/20.0f))).toInt )
//		}
//
////		cloudConnections = cloudPoints.zipWithIndex.map{ case(_,i) => if ( i == 0 ) { List(1) } else if ( i == numCloudPoints - 1 ) { List(numCloudPoints-2) } else { List(i-1,i+1) } }
//		cloudConnections = cloudPoints.zipWithIndex.map{ case(_,i) => (0 until numCloudPoints).toList.filter(j => j != i) }
//		equilibriumDistances = cloudConnections.zipWithIndex.map{ case (cons,i) => cons.map( con => (cloudPoints(i) - cloudPoints(con)).lengthSafe ) }
	}


	var t = 0.0f
	def tick(timeElapsed: UnitOfTime): Unit = {
//		val f = timeElapsed.inSeconds
//		val WD = world.aux[WindData]
//		cloudPoints = cloudPoints.map{ p =>
//			val f = WD.windDirection(p.x.toInt,p.y.toInt)
//			p + Vec2f(cosf(f),sinf(f)) * cloudSpeed }
//
//		cloudPoints = cloudPoints.zip(cloudConnections).zip(equilibriumDistances).map{ case ((point,connections),equiDistances) => {
//			var tmp = point
//			for ( (connection,equiDist) <- connections.zip(equiDistances) ) {
//				val delta = cloudPoints(connection) - point
//				val deltaLength = delta.lengthSafe
//				val deltaNormalized = delta.normalize
//
//				val scaleTo = (deltaLength - equiDist) * 0.0015f * (1.0f - ( equiDist / 250.0f)).max(0.0f)
//
//				tmp += deltaNormalized * scaleTo
//			}
//			tmp
//		}}
//
//		GameEngine.time += timeElapsed
	}

	def draw(graphics: GraphicsHelper): Unit = {
		val WD = world.aux[WindData]

		graphicsEngine.pov.asInstanceOf[TopDownCamera].moveSpeed = Vec3f(0.1f)

		val reg = world.worldRegionAsSpatialRegion
		val size = 18.0f
		graphics.drawQuad(Vec2f.Zero,Vec2f(size,size),heightmapImage,Vec4f.One)

		val vecSize = 0.125f
		for ( x <- reg.minX until reg.maxX by 8 optimized ; y <- reg.minY until reg.maxY by 8 optimized ) {
			val gx = (((x - reg.minX) / reg.dimensions.x.toFloat) - 0.5f) * size
			val gy = (((y - reg.minY) / reg.dimensions.y.toFloat) - 0.5f) * size

			val wv = WD.windVector(x,y)
			val rot = atan2f(wv.y,wv.x)
			val l = wv.lengthSafe.max(0.01f)
			graphics.drawQuad(Vec2f(gx,gy),rot,Vec2f(vecSize * l),vecImage,Vec4f.One)
		}

		val clouds = world.entitiesOfType[Cloud]
		for ( cloud <- clouds ; p = cloud.position ) {
			val gx = (((p.x - reg.minX) / reg.dimensions.x.toFloat) - 0.5f) * size
			val gy = (((p.y - reg.minY) / reg.dimensions.y.toFloat) - 0.5f) * size
			graphics.drawQuad(Vec2f(gx,gy),0.0f,Vec2f(vecSize*5.0f),image("hellir/graphics/particles/star_1.png"),Vec4f.One)
		}
	}

	def onKeyPress(kpe: KeyPressEvent): Unit = {
		if ( kpe.key == Keyboard.KEY_EQUALS ){
			gameEngine.timescale *= 2.0f
		} else if ( kpe.key == Keyboard.KEY_MINUS ) {
			gameEngine.timescale /= 2.0f
		}
	}
	def onKeyRelease(kpe: KeyReleaseEvent): Unit = {}
	def onMousePress(mpe: MousePressEvent): Unit = {


	}
	def onMouseRelease(mre: MouseReleaseEvent): Unit = {}
	def onMouseMove(mme: MouseMoveEvent): Unit = {}
	def onMouseDrag(mde: MouseDragEvent): Unit = {}
}
