package arx.axistential.testbed

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/21/14
 * Time: 6:02 PM
 */

import arx.Prelude._
import arx.application.Application
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.testbed.TestSpriteRendererComponent.Sprite
import arx.axistential.testbed.TestSpriteRendererComponent.SpriteData
import arx.axistential.testbed.advancedlighting.AdvancedLocalLightRendering
import arx.core.gen.ArxGenerators._
import arx.core.gen.SimplexNoise
import arx.core.vec._
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGridWindow
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.GameEntity.EnrichedGameEntity
import arx.tmp.game.logic.entities.LightPositionChanged
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.world.SpatialRegion
import arx.tmp.game.logic.world.data.LightData
import arx.resource.ResourceManager

import scalaxy.loops._

class WorldRenderingTestbed extends BareBonesTestbed {
	override def setUpGraphicsEngine(): Unit = {
		graphicsEngine addComponent new AdvancedLocalLightRendering

		graphicsEngine addComponent pio[TestSpriteRendererComponent]

		graphicsEngine addComponent new DynamicGraphicsComponent {
			override def bucketRequirements = RenderBucketRequirements(UIAttributeProfile,ResourceManager.shader("shaders/axistential/GameUI"))
			override def bucketIdentifier: Symbol = 'entity
			override protected def update(f: Float): Unit = {
				CommonRendering.billboardQuad(getBucket,light.exactLightLocation,Vec2f.One,Vec4f.One,image("axis/entities/items/textures/bonfire.png"))
			}
		}

		graphicsEngine.pov match {
			case anth: Camera =>
				anth.moveSpeed *= 0.25f
				anth.turnSpeed *= 0.5f
				anth.eye = ObjectCoord(-16.0f,-16.0f,17.0f)
				anth.angles = Vec2f(0.737f,0.737f)
		}
	}

	val seed = System.currentTimeMillis()
	var lastGenHashCode = 0

	var light = new GameEntity with TLightSource {
		var position = ObjectCoord(0.0f,0.0f,15.0f)
		override def lightLocation: VoxelCoord = position.toVoxelCoord
		override def exactLightLocation = position
		def move (dx:Float,dy:Float,dz:Float) {
			val oldP = lightLocation
			position += Vec3f(dx,dy,dz)
			val newP = lightLocation
			if (oldP != newP) {
				val tmp = new EnrichedGameEntity(this)
				tmp.eventHappened( LightPositionChanged(oldP,newP) )
			}
		}
	}

	override def setUpGameEngine(): Unit = {
		gameEngine addComponent new LightingComponent

		gameEngine.world.addEntity(light)

		val SD = gameEngine.world.aux[SpriteData]

		val pineTree = "axis/entities/plants/textures/pineTree/"
		val img = image(pineTree + "diffuse.png")
		val dim = CommonRendering.scaleToImage(Vec2f(5.0f,8.0f),img)
		SD.sprites = List( Sprite(Vec3f(-5.0f,-5.0f,0.0f), dim, pineTree) )
	}

	override def update(f: Float): Unit = {
		super.update (f)

		val world = gameEngine.activeWorld
		val terrain = world.aux[TerrainData]

		val gen = generator
		if (Application.ticks % 100 == 0) {
			terrain.materialGrid.modificationBlock(SpatialRegion(VoxelCoord.Center,Vec3i(32))) {
				for ( x <- -16 to 16 ; y <- -16 to 16 ; z <- -8 to gen(x,y).toInt ) {
					terrain.setMaterialAt(VoxelCoord.Center + Vec3i(x,y,z), Material.withName("soil"))
				}
				for (x <- -3 to 3 ; y <- -3 to 3 ; z <- -8 to 5 ) {
					terrain.setMaterialAt(VoxelCoord.Center + Vec3i(x,y,z), Material.withName("soil"))
				}
			}
		}

		for ( sprite <- world.aux[SpriteData].sprites ) {
			var base = Vec3f(sprite.position)

			while (terrain.isOccupied(ObjectCoord(base - Vec3f(0.0f,0.0f,0.0f)).toVoxelCoord)){
				base = base + Vec3f.UnitZ
			}
			sprite.position = base
		}
	}

	def generator = {
		Scale(0.05f) >> Simplex(new SimplexNoise(seed)) >> Mult(4.0f)
	}


	override def setUpUI(): Unit = {
		gameController.addTopLevelMode(new TGameMode {
			onEvent {
				case kpe : KeyPressEvent => {
					if ( kpe.key == Keyboard.KEY_J ) {
						light.move(-0.1f,0.0f,0.0f)
					} else if ( kpe.key == Keyboard.KEY_L ) {
						light.move(0.1f,0.0f,0.0f)
					} else if ( kpe.key == Keyboard.KEY_I ) {
						light.move(0.0f,0.0f,0.1f)
					} else if ( kpe.key == Keyboard.KEY_M) {
						light.move(-0.0f,0.0f,-0.1f)
					}
				}
			}
		})
	}
}

abstract class TestWorldRendererComponent extends DynamicGraphicsComponent {
	override def bucketIdentifier: Symbol = 'GeneralWorldRenderer

	def drawVoxelFace(x: Int, y: Int, z: Int, ox: Float, oy: Float, oz: Float, q: Int, terrainWindow: GenericTaleaGridWindow[Byte, ByteTalea], lightWindow: GenericTaleaGridWindow[Byte, LightData.LightTaleaType], world: World, bucket: RenderBucket)


	override protected def update(f: Float): Unit = {
		val bucket = getBucket

		val world = gameEngine.world
		val terrain = world.aux[TerrainData]
		val lighting = world.aux[LightData]

		val drawnRegion = SpatialRegion.apply(VoxelCoord.Center,Vec3i(48))
		for ( terrainTalea <- terrain.materialGrid.allInRegionInclusive(drawnRegion) ) {
//			if (terrain.materialGrid.definedAt(terrainTalea.position)) {
				val tpos = terrainTalea.position

				val terrainWindow = terrain.materialGrid.windowCenteredOnTaleaAt(terrainTalea.position,readOnly=true)
				val lightWindow = lighting.globalLighting(0).windowCenteredOnTaleaAt(terrainTalea.position, readOnly=true)

				for ( x <- 0 until Talea.dimension optimized ;
						y <- 0 until Talea.dimension optimized ;
						z <- 0 until Talea.dimension optimized )
				{
					val ox = ObjectCoord.fromVoxelCoordX(tpos.x + x)
					val oy = ObjectCoord.fromVoxelCoordY(tpos.y + y)
					val oz = ObjectCoord.fromVoxelCoordZ(tpos.z + z)

					val terrainValue = terrainWindow(x,y,z)
					if ( terrainValue > 0 ) {
						for ( q <- 0 until 6 optimized ) {
							val ax = x + cardinalsX(q)
							val ay = y + cardinalsY(q)
							val az = z + cardinalsZ(q)

							val adj = terrainWindow(ax,ay,az)
							if ( adj <= 0 ) {
								drawVoxelFace(x,y,z,ox,oy,oz,q,terrainWindow,lightWindow,world,bucket)
							}
						}
					}
				}
//			}
		}
	}
}


abstract class TestSpriteRendererComponent extends DynamicGraphicsComponent {
	override def bucketIdentifier: Symbol = 'TestSpriteRenderer

	override protected def update(f: Float): Unit = {
		world.aux[SpriteData].sprites.foreach( s => drawSprite(getBucket,s) )
	}

	def drawSprite (bucket : RenderBucket,sprite : Sprite)
}

object TestSpriteRendererComponent {
	case class Sprite (var position : ReadVec3f, dimensions : ReadVec2f, resource : String)
	class SpriteData extends TWorldAuxData {
		var sprites = List[Sprite]()
	}
}

class TestWorldRenderer extends TestWorldRendererComponent {
	lazy val graphicsInfoProvider = pio[TGameEntityGraphicsInfoProvider]

	override def bucketRequirements =
		RenderBucketRequirements(SimpleAttributeProfile,ResourceManager.shader("shaders/Simple"))

	override def drawVoxelFace(x: Int, y: Int, z: Int, ox: Float, oy: Float, oz: Float, q: Int,
										terrainWindow: GenericTaleaGridWindow[Byte, ByteTalea],
										lightWindow: GenericTaleaGridWindow[Byte, LightData.LightTaleaType],
										world: World,
										bucket: RenderBucket): Unit =
	{
		val ax = x + cardinalsX(q)
		val ay = y + cardinalsY(q)
		val az = z + cardinalsZ(q)

		val terrain = world.aux[TerrainData]
		val ginfo = graphicsInfoProvider.graphicsInfoFor(terrain.materialForByte(terrainWindow(x,y,z)))
		val texture = ginfo.icon
		val tc = bucket.textureBlock(texture)

		val vi = bucket.incrementVertexOffset(4)
		val ii = bucket.incrementIndexOffset(6)
		val vbo = bucket.vbo

		val lightLevel = CommonRendering.lightMults(lightWindow(ax,ay,az))

		val coords = Cardinals.cubePoints(q)
		for ( k <- 0 until 4 optimized ) {
			vbo.setA(SimpleAttributeProfile.VertexAttribute,vi + k,coords(k).x + ox, coords(k).y + oy, coords(k).z + oz)
			vbo.setAbf(SimpleAttributeProfile.ColorAttribute,vi + k,lightLevel,lightLevel,lightLevel,1.0f,127)
			vbo.setA(SimpleAttributeProfile.TexCoordAttribute,vi + k,tc(k))
		}
		vbo.setIQuad(ii,vi)
	}
}