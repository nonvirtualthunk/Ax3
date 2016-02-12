package arx.axistential.testbed.thermo.application

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/21/13
 * Time: 8:42 AM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.Material
import arx.axistential.game.components.FluidGameComponent
import arx.axistential.game.components.WeatherComponent
import arx.axistential.game.data.world.GranularData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.generators.botanical.VegetationGenerator
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.meteorological.WindWorldGenerator
import arx.axistential.game.world.generation.generators.RawTerrainGenerator
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.axistential.graphics.components.SkyboxComponent
import arx.axistential.graphics.components.ai.GoalGraphicsComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.graphics.components.fluid.FluidViewerComponent2
import arx.axistential.graphics.components.granular.GranularViewerComponent
import arx.axistential.graphics.components.weather.CloudGameEngineComponent
import arx.axistential.graphics.components.weather.WeatherGraphicsComponent
import arx.axistential.testbed.thermo.logic.ThermodynamicsGameComponent
import arx.core.mathutil.WeightedDistribution
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.world.SpatialRegion

import scalaxy.loops._

class ThermoWorldTestbed extends BareBonesTestbed {
	var fluidComp = new FluidGameComponent

	def setUpGameEngine(): Unit = {
		val world = gameEngine.activeWorld

		val seed = System.currentTimeMillis()
		val desc = new WorldGenerationDescription(
			List(
				new TopologyGenerator,
				new RawTerrainGenerator,
				new GeologicalFeatureGenerator,
				new VegetationGenerator,
				new WindWorldGenerator
			),
			Vec3i(700,700,768)
			,seed
		)
		println(s"Seed : $seed")

		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)


		gameEngine.addComponent( new LightingComponent )
		gameEngine.addComponent( fluidComp )
		gameEngine.addComponent( new CloudGameEngineComponent )
//		gameEngine.addComponent( new SnowComponent )
		gameEngine.addComponent( new ThermodynamicsGameComponent )
		gameEngine.addComponent( new WeatherComponent )


		val v = VoxelCoord.Center.minusX(150)
		val z = world.aux[TopologicalData].heightmap(v)
//		val heatSource = new GameEntity with THeatSource {
//			def position: VoxelCoord = v.plusZ(z)
//			heatStrength = 100
//		}
		val heatSource = ItemArchetype.archetypeWithName("bonfire").createInstance
		heatSource.position = v.plusZ(z).toObjectCoord
		world.addEntity(heatSource)
		//		fluidComp.isActive = false
	}

	val EVC2 = new EnvironmentViewerComponent2

	def setUpGraphicsEngine(): Unit = {
		graphicsEngine.addComponent( EVC2 )
		graphicsEngine.addComponent( new PhysicalEntityViewerComponent )
		graphicsEngine.addComponent( new SkyboxComponent )
		graphicsEngine.addComponent( new FluidViewerComponent2 )
		graphicsEngine.addComponent( new GranularViewerComponent )
		graphicsEngine.addComponent( new ModeUIGraphicsComponent(gameController.modeStack,"axistential") )
		graphicsEngine addComponent new WeatherGraphicsComponent
		graphicsEngine addComponent new GoalGraphicsComponent

		val camera = new AnthologiconCamera(Vec3f(-60,0.0f,5.0f))
		camera.useGlobalUp = true
		camera.viewDistance = 400.0f
		camera.moveSpeed *= 0.5f
		graphicsEngine.pov = camera

	}

	def setUpUI(): Unit = {
		gameController.addTopLevelMode(new TestbedGameMode)
		//		windowingSystem.giveFocusTo(console.inputWidget)
	}

	class TestbedGameMode extends TGameMode {
		var mode : Int = 0
		var originPoint : Option[VoxelCoord] = None
		def currentVolume = {
			val (start,end) = currentWorldMouseLine
			graphicsEngine.intersectWorld(start,end) match {
				case Some(WorldIntersectionResult(voxel,point,side)) => {
					originPoint match {
						case None => (voxel + dirvec(side),voxel + dirvec(side))
						case Some(origin) => {
							val end = voxel + dirvec(side)
							val min = VoxelCoord(origin.min(end))
							val max = VoxelCoord(origin.max(end))

							min -> max
						}
					}
				}
				case _ => VoxelCoord.Sentinel -> VoxelCoord.Sentinel
			}
		}


		onEvent {
			case kpe : KeyPressEvent => {
				if ( kpe.key == Keyboard.KEY_EQUALS ) { gameEngine.timescale *= 2.0f }
				else if ( kpe.key == Keyboard.KEY_MINUS ) { gameEngine.timescale /= 2.0f }
			}
			case wme : WorldMousePressEvent => {
				val (start,end) = currentWorldMouseLine
				graphicsEngine.intersectWorld(start,end) match {
					case Some(WorldIntersectionResult(voxel,point,side)) => {
						originPoint = originPoint match {
							case Some(origin) => {
								val (min,max) = currentVolume

								val TD = world.aux[TerrainData]
								TD.materialGrid.modificationBlock(SpatialRegion.fromCorners(min,max)) {
									for ( v <- min to max ) {
										TD.setMaterialAt(VoxelCoord(v), Material.withName("stone") )
									}
								}

								None
							}
							case None => {
								Some(voxel + dirvec(side))
							}
						}
					}
				}
			}
		}


		override def update(f: Float): Unit = {

		}

		override def drawUI(bucket: RenderBucket): Unit = {
			val (min,max) = currentVolume
			CommonRendering.drawVolume(bucket,min,max,Vec4f(1.0f,1.0f,1.0f,0.5f))
		}
	}
}

class SnowComponent extends GameEngineComponent {

	override def initialize(): Unit = {
//		val TD = world.aux[TerrainData]
//		val heightmap = world.aux[TopologicalData].heightmap
//		val depthmap = world.aux[TopologicalData].depthmap
//
//		val GD = world.aux[GranularData]
//
////		val snowByte = TD.coveringTypes( new Covering(Material.materialWithName("snow") ) )
//		val snowByte = TD.materialMapping( Material.withName("snow") )
//
//		import arx.core.gen.ArxGenerators._
//		val simplex = Scale(0.05f) >> Simplex(new SimplexNoise(System.currentTimeMillis())) >> Mult(2) >> Add(1) >> Clamp(0.1f,1.0f)
//
//		val reg = world.worldRegion
//		for ( x <- reg.minX until reg.maxX optimized ; y <- reg.minY until reg.maxY optimized ) {
//			val h = heightmap(x,y)
//			val d = depthmap(x,y)
//			if ( h > d ) {
//				val z = VoxelCoord.Center.z + h
////				TD.coveringGrid(x,y,z-1,Top) = snowByte
//				GD.materialGrid(x,y,z) = snowByte
//				GD.levelGrid(x,y,z) = (simplex(x,y) * 127).toByte
//			}
//		}
	}

	var countdown = 1.second
	def update(time: UnitOfTime): Unit = {
		countdown -= time
		if ( countdown <= zeroSeconds ) {
			countdown = 0.1.second

			val TD = world.aux[TerrainData]
			val heightmap = world.aux[TopologicalData].heightmap
			val depthmap = world.aux[TopologicalData].depthmap

			val GD = world.aux[GranularData]
			val reg = world.worldRegionAsSpatialRegion

			val directions = WeightedDistribution( 1.0f -> Vec2i.Zero, 0.4f -> Vec2i(-1,0) , 0.15f -> Vec2i(1,0) , 0.225f -> Vec2i(0,1), 0.225f -> Vec2i(0,-1) )
			val snowByte = TD.materialMapping( Material.withName("snow") )

			for ( i <- 0 until 400 optimized ) {
				var x = rand( reg.minX , reg.maxX )
				var y = rand( reg.minY , reg.maxY )
				var driftCountdown = 10

				if ( heightmap(x,y) > depthmap(x,y) ) {
					var z = reg.maxZ
					var continue = true
					while ( z > reg.minZ && continue ) {
						if ( TD.materialGrid(x,y,z - 1) != 0 ) {
							continue = false
						} else {
							z -= 1
							driftCountdown -= 1
							if ( driftCountdown <= 0 ) {
								driftCountdown = rand(8,12)
								val dd = directions.rand()

								if ( TD.materialGrid(x + dd.x,y + dd.y,z) == 0 ) {
									x += dd.x
									y += dd.y
								}
							}
						}
					}
					if ( ! continue ) {
						GD.levelGrid.modificationBlock(VoxelCoord(x,y,z)) {
							val cur = GD.levelGrid(x,y,z)
							GD.levelGrid(x,y,z) = (cur + 10).toByte
							GD.materialGrid(x,y,z) = snowByte
						}
					}
				}
			}
		}
	}
}

class SnowGen extends TWorldGenerator {
	/**
	 * Perform this component's generation, storing the results in the provided world.
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		if ( stage == WorldGenerator.Voxel && phase == WorldGenerator.Hydrological ) {
			val TD = world.aux[TerrainData]
			val heightmap = world.aux[TopologicalData].heightmap
			val depthmap = world.aux[TopologicalData].depthmap

			val snowByte = TD.coveringTypes( new Covering(Material.materialWithName("snow") ) )

			val reg = world.worldRegionAsSpatialRegion
			for ( x <- reg.minX until reg.maxX optimized ; y <- reg.minY until reg.maxY optimized ) {
				val h = heightmap(x,y)
  				val d = depthmap(x,y)
				if ( h > d ) {
					val z = VoxelCoord.Center.z + h
					TD.coveringGrid(x,y,z,Top) = snowByte
				}
			}
			Nil
		} else { Nil }
	}
}
