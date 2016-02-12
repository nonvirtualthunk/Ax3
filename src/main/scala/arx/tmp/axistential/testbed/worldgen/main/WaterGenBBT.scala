package arx.axistential.testbed.worldgen.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/9/13
 * Time: 7:24 PM
 */

import arx.Prelude._
import arx.axistential.game.data.world._
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.hydrological.RiverData
import arx.axistential.game.world.generation.generators.hydrological.SoilMoistureGenerator
import arx.axistential.game.world.generation.generators.RawTerrainGenerator
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.graphics.Image

import scalaxy.loops._

class WaterGenBBT extends BareBonesTestbed {
	var image : Image = Image.Sentinel

	def setUpGameEngine(): Unit = {
		val world = gameEngine.activeWorld

		val desc = new WorldGenerationDescription(
			List(
				new TopologyGenerator,
				new RawTerrainGenerator,
				new GeologicalFeatureGenerator,
//				new RiverGenerator,
				new SoilMoistureGenerator
//				new VegetationGenerator
			),
			Vec3i(1256,1256,512),
			5L
		)

		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)


		val GCT = new GameEngineComponent {
			var x = 0
			var y = 0
			var z = 0
			override def initialize () {
				x = world.worldRegionAsSpatialRegion.minX
				y = world.worldRegionAsSpatialRegion.minY
				z = world.worldRegionAsSpatialRegion.minZ
			}

			def update(time: UnitOfTime): Unit ={
				timeAndPrint("full iteration (1256,1256)") {
					val reg = world.worldRegionAsSpatialRegion
					var bit = false
					val TD = world.aux[TerrainData]
//					for ( x <- reg.minX until reg.maxX by 32 optimized ;
//							y <- reg.minY until reg.maxY by 32 optimized ;
//							z <- reg.minZ until reg.maxZ by 32 optimized )
//					{

						val window = TD.materialGrid.windowCenteredOnTaleaAt(VoxelCoord(x,y,z),readOnly=true)
//						if ( ! window.centerTalea.areAll(0) ) {
							for ( dx <- 0 until 32 optimized ; dy <- 0 until 32 optimized ; dz <- 0 until 32 optimized ) {
								bit = window(dx,dy,dz) > 0
							}
//						}
//					}
				}
			}
		}
		gameEngine.addComponent(GCT)

//		gameEngine.addComponent( new LightingComponent )
	}

	def setUpGraphicsEngine(): Unit = {
//		graphicsEngine.addComponent( new EnvironmentViewerComponent2 )
//		graphicsEngine.addComponent( new PhysicalEntityViewerComponent )
//		graphicsEngine.addComponent( new SkyboxComponent )
//		graphicsEngine.addComponent( new ModeUIGraphicsComponent(gameController.modeStack,"axistential") )

		val camera = new AnthologiconCamera(Vec3f(-60,0.0f,5.0f))
		camera.useGlobalUp = true
		camera.viewDistance = 400.0f
		graphicsEngine.pov = camera

	}

	def setUpUI(): Unit = {
		val world = gameEngine.activeWorld
		gameController.addTopLevelMode(new TestbedGameMode)
		//		windowingSystem.giveFocusTo(console.inputWidget)

		image = Image.withDimensions( world.worldRegionAsSpatialRegion.dimensions.x , world.worldRegionAsSpatialRegion.dimensions.y )
		updateImage()

		val w = new Widget(mainWidget)
		w.backgroundImage = image
		w.segmentedBackground = false
		w.width = 100.0f
		w.height = 100.0f
		w.x = (mainWidget.width - 100.0f) * 0.5f


	}


	override def update(f: Float): Unit = {
		super.update(f)

//		updateImage()
//		windowingSystem.mainTextureBlock.updateImage(image)
	}

	var counter = 0

	def updateImage () {
		counter += 100
		val world = gameEngine.activeWorld

		val heightmap = world.aux[TopologicalData].heightmap
		val depthmap = world.aux[TopologicalData].depthmap
		val rainfall = world.aux[SoilData].rainfall
		val scale = world.aux[ScaleData]
		val reg = world.worldRegionAsSpatialRegion

		for ( x <- reg.minX until reg.maxX optimized ; y <- reg.minY until reg.maxY optimized ) {
			val alpha = if ( heightmap(x,y) > depthmap(x,y) ) { 255 } else { 0 }
			val hp = ((heightmap(x,y) + 50) / (scale.MountainPeak.inVoxels)).clamp(0.0f,1.0f)
			val rp = clamp((rainfall(x,y) + 30.0f) / 60.0f,0.0f,1.0f)

			val r = ((rp) * 255).toInt
			val g = (rp* 255).toInt
			val b = ((rp) * 255).toInt

			val hi = (hp * 255).toInt
			image(x - reg.minX,y - reg.minY) = Vec4i(r,g,b,alpha)
		}

		//		var highestPoint : ReadVec2i = Vec2i(0,0)
		//		var highestElevation = 0
		//		for ( x <- reg.minX until reg.maxX ; y <- reg.minY until reg.maxY ) {
		//			if ( heightmap(x,y) > highestElevation ) {
		//				highestPoint = Vec2i(x,y)
		//				highestElevation = heightmap(x,y)
		//			}
		//		}

		val RD = world.aux[RiverData]
		for ( river <- RD.rivers ) {
			var i = 0
			for ( point <- river.originalCourse ; if i < counter ) {
				image(point.x - reg.minX,point.y - reg.minY) = Vec4i(10,10,200,255)
				i += 1
			}
		}
	}

	class TestbedGameMode extends TGameMode {
		override def drawUI(bucket: RenderBucket): Unit = {
//			CommonRendering.quad(bucket,Vec3f.Zero,Vec3f.UnitX,Vec3f.UnitY,Vec2f(10.0f,10.0f),Vec4f.One,bucket.textureBlock(image))
		}
	}
}

