package arx.axistential.game.world.generation.generators.hydrological

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/15/13
 * Time: 12:32 PM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.SoilData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.tmp.game.logic._
import arx.tmp.game.logic.datastructures._
import arx.tmp.game.logic.world.SpatialRegion

import scalaxy.loops._

class SoilMoistureGenerator extends TWorldGenerator {


	/**
	 * Perform this component's generation, storing the results in the provided world.
 *
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		import arx.core.gen.ArxGenerators._

		if ( phase == WorldGenerator.Hydrological ) {
			val hydroData = world.aux[SoilData]
			if ( stage == WorldGenerator.Heightmap ) {
				for ( cell <- hydroData.rainfall.allInRegionInclusive( world.worldRegionAsSpatialRegion ).par_ts(taskSupport) ) {
					val gen = Translate(cell.x,cell.y,0.0f) >> Scale(0.002f) >> Fractal(2)(Simplex(simplexGenerator)) >> Mult(10.0f)

					for ( x <- 0 until GridCell2D.dimension optimized ;
							y <- 0 until GridCell2D.dimension optimized )
					{
						cell(x,y) = gen(x,y).toByte
					}
				}
			}
		}
		Nil
	}
}

class SoilMoistureGameComponent extends GameEngineComponent {
	dependencies ::= classOf[TVoxelIterationGameComponent]

	override def initialize(): Unit = {
		val reg = world.worldRegionAsSpatialRegion
		cellIterator = MutableVoxelCoord(( reg.lowerCorner >> Talea.dimensionPo2) << Talea.dimensionPo2)
		lastReset = world.time
	}

	var lastReset = zeroSeconds
	var cellIterator = MutableVoxelCoord(0,0,0)
	val cardinals = Array( dirvec(Bottom), dirvec(Left), dirvec(Right), dirvec(Front), dirvec(Back), dirvec(Top) )

	def update(time: UnitOfTime): Unit = {
		val TD = world.aux[TerrainData]
		val materialGrid = TD.materialGrid.windowCenteredOnTaleaAt(cellIterator,readOnly = true)
		val materials = TD.materialMapping
		val HD = world.aux[SoilData]
		val moistureGrid = HD.deltaSoilMoisture.windowCenteredOnTaleaAt(cellIterator,readOnly = false)

		val maxToAdj = 80
		val dropoff = 10
		val maxFlow = 20

		for ( z <- 0 until Talea.dimension optimized ;
				y <- 0 until Talea.dimension optimized ;
				x <- 0 until Talea.dimension optimized )
		{
			val matByte = materialGrid(x,y,z)
			if ( matByte > 0 ) {
				val mat = materials(matByte)
				if ( mat.materialTypes.contains( Material.Soil ) ) {
					var hydro = moistureGrid(x,y,z)
					var effHydro = math.min(maxToAdj,hydro)

					for ( q <- 0 until 6 optimized ) {
						val ax = cardinals(q).x + x
						val ay = cardinals(q).y + y
						val az = cardinals(q).z + z

						val adjMatByte = materialGrid(x,y,z)
						if ( adjMatByte > 0 ) {
							val adjMat = materials(adjMatByte)
							if ( adjMat.materialTypes( Material.Soil ) ) {
								val adjHydro = moistureGrid(ax,ay,az)

								val delta = adjHydro - (effHydro - dropoff)
								if ( delta < 0 ) {
									val flow = math.min(delta, maxFlow)

									moistureGrid(ax,ay,az) = (moistureGrid(ax,ay,az) + flow).toByte
									if ( hydro != 127 ) {
										moistureGrid(x,y,z) = (moistureGrid(x,y,z) - flow).toByte
										hydro = (hydro - flow).toByte
										effHydro = (effHydro - flow).toByte
									}
								}
							}
						}
					}
				}
			}
		}

		cellIterator.x += Talea.dimension
		if ( cellIterator.x > world.worldRegionAsSpatialRegion.maxX ) {
			cellIterator.x = world.worldRegionAsSpatialRegion.minX
			cellIterator.y += Talea.dimension
			if ( cellIterator.y > world.worldRegionAsSpatialRegion.maxY ) {
				cellIterator.y = world.worldRegionAsSpatialRegion.minY
				cellIterator.z += Talea.dimension
				if ( cellIterator.z > world.worldRegionAsSpatialRegion.maxZ ) {
					cellIterator.z = world.worldRegionAsSpatialRegion.minZ
					println("mark")
					lastReset = world.time
				}
			}
		}
	}
}

trait TVoxelIterationGameComponent extends GameEngineComponent {
	def addVoxelIterationCallback ( callback : (VoxelIterator) => Unit )
}
class VoxelIterationGameComponent extends TVoxelIterationGameComponent {
	var lastTime = zeroSeconds
	var iterator = new VoxelIterator

	var terrainData : TerrainData = null
	var callbacks : List[(VoxelIterator) => Unit] = Nil

	override def initialize () {
		lastTime = world.time

		terrainData = world.aux[TerrainData]

		iterator._x = world.worldRegionAsSpatialRegion.minX
		iterator._y = world.worldRegionAsSpatialRegion.minY
		iterator._z = world.worldRegionAsSpatialRegion.minZ
	}

	def update(time: UnitOfTime): Unit = {

	}

	def addVoxelIterationCallback(callback: (VoxelIterator) => Unit): Unit = {
		callbacks ::= callback
	}
}

class LoopingTaleaIterator[T,TT <: ITalea[T]] ( grid : GenericTaleaGrid[T,TT], region : SpatialRegion ) {
	val v = MutableVoxelCoord(region.minX,region.minY,region.minZ)
	def x = v.x
	def y = v.y
	def z = v.z

	/**
	 * @return true if this advancement rolled back over to the beginning, false otherwise
	 */
	def advance () : Boolean = {
		var ret = false
		v.x += Talea.dimension
		if ( v.x > region.maxX ) {
			v.x = region.minX
			v.y += Talea.dimension
			if ( v.y > region.maxY ) {
				v.y = region.minY
				v.z += Talea.dimension
				if ( v.z > region.maxZ ) {
					v.z = region.minZ
					ret = true
				}
			}
		}
		ret
	}
}

class VoxelIterator {
	var _x : Int = 0
	var _y : Int = 0
	var _z : Int = 0
	def x = _x
	def y = _y
	def z = _z

	var _timeElapsed = zeroSeconds
	def timeElapsed = _timeElapsed

	var _materialByte : Byte = 0.toByte
	def materialByte = _materialByte
	var _material : Material = Material.Sentinel
	def material = _material
}