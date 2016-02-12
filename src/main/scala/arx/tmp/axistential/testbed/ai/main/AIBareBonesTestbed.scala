package arx.axistential.testbed.ai.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.ai.AI
import arx.axistential.ai.AIGameComponent
import arx.axistential.ai.TAIGroup
import arx.axistential.game.archetypes._
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.components._
import arx.axistential.game.components.construction.StructuralSupportGameComponent
import arx.axistential.game.components.physics.BulletPhysicsComponent
import arx.axistential.game.components.physics.DebugData
import arx.axistential.game.components.physics.VoxelDebugGraphicsComponent
import arx.axistential.game.components.physics.btPhysicsEntityData
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.logic.ai.goals._
import arx.axistential.game.logic.ai.passivegoals.FleeFromEnemies
import arx.axistential.game.logic.general.Location.VoxelLocation
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.logic.requirements.MaterialTypeDescriptor
import arx.axistential.graphics.components.ai.GoalGraphicsComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.graphics.components.environmentviewer.viewlayers.StructuralSupportViewLayer
import arx.axistential.graphics.components.environmentviewer.viewlayers.UIViewLayerGraphicsComponent
import arx.axistential.testbed.ai.main.AIBareBonesTestbed.Enemy
import arx.axistential.testbed.ai.main.AIBareBonesTestbed.VoidMind
import arx.axistential.testbed.ai.main.AIBareBonesTestbed.Worker
import arx.core.GeneralityLevel
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.Vec4f
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.AVBO
import arx.graphics.GL
import arx.graphics.TextureBlock
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11

class AIBareBonesTestbed extends BareBonesTestbed {
	import arx.axistential.testbed.ai.main.AIBareBonesTestbed._

	def setUpGameEngine(): Unit = {
		val world = gameEngine.activeWorld

		world.aux[TerrainData]._setFromFunction( (x,y,z) => {
			if ( z < VoxelCoord.Center.z ) {
				CommonMaterials.SmoothedStone
			} else if ( z == VoxelCoord.Center.z + 30 && (x - VoxelCoord.Center.x).abs < 10 ) {
				CommonMaterials.SmoothedStone
			} else if ( z < VoxelCoord.Center.z + 4 && x == VoxelCoord.Center.x + 10 ) {
				CommonMaterials.ConstructedStone
			} else if ( z < VoxelCoord.Center.z + 6 && x == VoxelCoord.Center.x + 10 && y < VoxelCoord.Center.y ) {
				CommonMaterials.ConstructedStone
			} else if ( z < VoxelCoord.Center.z + 6 && x == VoxelCoord.Center.x + 15 ) {
				CommonMaterials.ConstructedStone
			} else if ( z == VoxelCoord.Center.z && ((VoxelCoord.Center.x - x).abs == 5 || (VoxelCoord.Center.y - y).abs == 5) ) {
				CommonMaterials.ConstructedStone
			} else {
				Material.Sentinel
			}
		} , 30)

		val workers = fillList(3)(i => {
			val w = new Worker
			w.position = VoxelCoord.Center.minusX(10 + i*3).plusZ(2).toObjectCoord
			world.addEntity(w)

			w.passiveGoals ::= new FleeFromEnemies
			w.passiveGoals ::= new EmptyInventoryGoal

			w
		})


		val colony = new Colony
		workers.foreach( colony.addEntity )
		world.addEntity(colony)

		val pile = ItemArchetype.archetypeWithName("pile").createInstance
		pile.position = ObjectCoord(0f,4f,1.meter.inVoxels * 0.5f)
		world.addEntity(pile)
		colony.addEntity(pile)



		val toDig = (for( x <- -15 until -10 ; y <- -15 until -10 ; z <- -4 until 0 ) yield { VoxelCoord.Center + Vec3i(x,y,z) }).toList.sortBy( _.z * -1 )
		colony.addGoal( DigGoal(toDig) )

		val toDig2 = (for( x <- -15 until -10 ; y <- -25 until -20 ; z <- -4 until 0 ) yield { VoxelCoord.Center + Vec3i(x,y,z) }).toList.sortBy( _.z * -1 )
		colony.addGoal( DigGoal(toDig2) )
		
		val toBuild = for (y <- -20 until -10; z <- 0 until 5) yield {
			VoxelCoord.Center + Vec3i (-20, y, z)
		}
		val buildGoal = BuildVoxelGoal(toBuild.toList,new MaterialTypeDescriptor(Material.Stone))
		buildGoal._priority = AI.Priority.High
		colony.addGoal( buildGoal )
		
		for ( i <- 0 until 40 ) { pile.aux[InventoryData].holdEntityForced( Material.withName("stone").createInstance ) }

		val craftRecipe = Recipe.recipesByOutput.get( ItemArchetype.archetypeWithName("statue") ).head
		val craftGoal = CraftRecipeGoal(craftRecipe,VoxelLocation(VoxelCoord.Center + (3,0,(2.meters.inVoxels * 0.5f).toInt),world),None)
		craftGoal._priority = AI.Priority.High
		colony.addGoal( craftGoal )

		val voidMind = new VoidMind
		world.addEntity(voidMind)

		gameEngine.addComponent( new LightingComponent )
		gameEngine.addComponent( new AIGameComponent )
		gameEngine.addComponent( new EnemyAwarenessComponent )
//		gameEngine.addComponent( new PhysicsComponent )
		gameEngine.addComponent( new BulletPhysicsComponent )
		gameEngine.addComponent( new StructuralSupportGameComponent )
	}

	def setUpGraphicsEngine(): Unit = {
		gameEngine.world.aux[DebugData].active = true

		val envViewer = new EnvironmentViewerComponent2
		envViewer.addViewLayer( new StructuralSupportViewLayer )
		graphicsEngine.addComponent( envViewer )
		graphicsEngine.addComponent( new PhysicalEntityViewerComponent )
		graphicsEngine.addComponent( new GoalGraphicsComponent )
		graphicsEngine.addComponent( new ModeUIGraphicsComponent(gameController.modeStack,"axistential") )
//		graphicsEngine.addComponent( new BulletPhysicsDebugGraphicsComponent )
		graphicsEngine.addComponent( new VoxelDebugGraphicsComponent )
		graphicsEngine.addComponent( new UIViewLayerGraphicsComponent )

		val camera = new AnthologiconCamera(Vec3f(-60,0.0f,5.0f))
		camera.useGlobalUp = true
		graphicsEngine.pov = camera

	}

	def setUpUI(): Unit = {
		gameController.addTopLevelMode(new TestbedGameMode)
//		windowingSystem.giveFocusTo(console.inputWidget)

//		val cw = new ConsoleWidget(null)
//		cw.interpreter.bind("gameEngine",gameEngine)
//		cw.interpreter.bind("world",world)
//		windowingSystem.addTopLevelWidget(cw)
	}

	var z = 0
	override def update(f: Float): Unit = {
		super.update (f)


		val terrain = gameEngine.activeWorld.aux[TerrainData]
//		if ( z % 20 == 0 ) {
//			val v = VoxelCoord.Center + Vec3i(15 + z / 40,15,z/20)
//			terrain.materialGrid.modificationBlock(v) {
//				terrain.setMaterialAt(v,CommonMaterials.ConstructedStone)
//			}
//		}
//		z += 1

//		val rad = 20
//		val circleVoxels = (for ( theta <- 0.0f until math.Pi.toFloat * 2.0f by 0.1f ) yield {
//			VoxelCoord.Center + Vec3i((cosf(theta) * rad).toInt,(sinf(theta) * rad).toInt,1)
//		}).toList.distinct
//
//		terrain.materialGrid.modificationBlock(circleVoxels) {
//			for ( (vox,index) <- circleVoxels.zipWithIndex ) {
//				if ( index == (z/3) % circleVoxels.size ) {
//					terrain.setMaterialAt(vox,CommonMaterials.ConstructedStone)
//					terrain.setMaterialAt(vox.plusZ(1),CommonMaterials.ConstructedStone)
//
//				} else {
//					terrain.setMaterialAt(vox,Material.Sentinel)
//					terrain.setMaterialAt(vox.plusZ(1),Material.Sentinel)
//				}
//			}
//		}

		z += 1
	}
}

protected[main] class TestbedGameMode extends TGameMode {
	lazy val voidMind = world.entitiesOfType[VoidMind].head
	onEvent {
		case WorldMousePressEvent(_,start,end,modifiers) => {
			graphicsEngine.intersectWorld(start,end) match {
				case Some(WorldIntersectionResult(voxel,point,side)) => {
//					val enemy = new Enemy
//					enemy.position = point.plusZ(2.01f)
//					world.addEntity(enemy)
//					voidMind.addEntity(enemy)
					println("Lighting is : " + world.aux[LightData].globalLighting(0)(voxel + dirvec(side)))
					println("\tInside vox lighting is : " + world.aux[LightData].globalLighting(0)(voxel))
//					world.aux[TerrainData]
				}
				case _ =>
			}
		}
		case KeyPressEvent(key,modifiers) => {
			if ( key == Keyboard.KEY_C ) {
				world.entitiesOfType[Enemy].foreach( e => e.die() )
			} else if ( key == Keyboard.KEY_M ) {
				world.entities.ofType[CreatureEntity].foreach( e => e.position = e.position.minusZ(1))
			} else if ( key == Keyboard.KEY_J ) {
				world.entities.ofType[CreatureEntity].foreach(
					e => {
						e.position = e.position.plusZ(25)
						e.auxDataOpt[btPhysicsEntityData] match {
							case Some(pd) => pd.rigidBody.translate(new Vector3(0,0,25))
							case None =>
						}
					}
				)
			} else if ( key == Keyboard.KEY_K ) {
				world.entities.ofType[CreatureEntity].foreach( e => e.position = e.position.plusZ(0.1f) )
			} else if ( key == Keyboard.KEY_EQUALS ) {
				gameEngine.increaseTimescale()
			} else if ( key == Keyboard.KEY_MINUS ) {
				gameEngine.decreaseTimescale()
			}
		}
	}

	override def drawUI(bucket: RenderBucket): Unit = {
		world.entitiesOfType[Worker].flatMap( _.activeLeafGoal ).ofType[MoveGoal].foreach( mv => {
			val tmpBeaconPoints = mv.beaconPoints
			val tmpBeaconIndex = mv.beaconIndex
			
//			val tc = bucket.textureBlock( image("ui/cancel.png") )
//			val tc2 = bucket.textureBlock( image("ui/forwardArrow.png") )
//			if ( tmpBeaconIndex < tmpBeaconPoints.size ) {
//				CommonRendering.billboardQuad(bucket,tmpBeaconPoints(tmpBeaconIndex),Vec2f(0.5f),Vec4f.One,CommonRendering.passthroughLightResult,tc)
//
//				CommonRendering.billboardQuad(bucket,tmpBeaconPoints(tmpBeaconIndex-1),Vec2f(0.5f),Vec4f.One,CommonRendering.passthroughLightResult,tc2)
//
//				if ( tmpBeaconIndex > 1 ) {
//					CommonRendering.billboardQuad(bucket,tmpBeaconPoints(tmpBeaconIndex-2),Vec2f(0.35f),Vec4f.One,CommonRendering.passthroughLightResult,tc2)
//				}
//			}
		})

		val (start,end) = this.currentWorldMouseLine
		graphicsEngine.intersectWorld(start,end) match {
			case Some(WorldIntersectionResult(voxel,point,side)) => {
				val tc = bucket.textureBlock( image("ui/monochromeIndicatorArrow.png") )
				CommonRendering.billboardQuad(bucket,point + Vec3f(Cardinals.cardinals(side)),Vec2f.One,Vec4f.One,CommonRendering.passthroughLightResult,tc)

				val terrain = world.aux[TerrainData]



//				AxisSearcher.pathTo( PathQuery(
//					world.entitiesOfType[Worker].head.footPosition.toVoxelCoord,
//					voxel + Cardinals.cardinals(side),
//					Vec3i.One,
//					200,
//					(v1,v2) => (v2 - v1).length,
//					(v) => true,
//					Searcher.defaultHeuristic,
//					(v) => terrain.isSolid(v),
//					(v) => terrain.isSolid(v.minusZ(1))
//				)) match {
//					case Some(vlist) => {
//						for ( v <- vlist ) {
//							CommonRendering.billboardQuad(bucket,v.toObjectCoord,Vec2f.One,Vec4f(0.2f,0.2f,0.7f,1.0f),CommonRendering.passthroughLightResult,tc)
//						}
//					}
//					case None =>
//				}
			}
			case None =>
		}
	}
}

object AIBareBonesTestbed {
	class Worker extends CreatureEntity {
		species = AnimalSpecies.archetypeWithName("human")
	}

	class Enemy extends CreatureEntity {
	}

	class Colony extends GameEntity with TAIGroup {

	}

	class VoidMind extends GameEntity with TAIGroup {

	}

	class WorkerGIP extends TGameEntityGraphicsStructor {
		def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Specific

		override def graphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = gameEntity match {
//			case worker : Worker => {
//				Some(BillboardGraphicsInfo(List("axis/entities/human/farmer/Untitled 1.png"),Vec4f.One))
//			}
			case enemy : Enemy => {
				enemy.alive.resolve() match {
					case true => Some(BillboardGraphicsInfo(List("axis/entities/animals/voidcreature/voidcreature.png"),Vec4f.One))
					case false => Some(NoGraphicsInfo)
				}
			}
			case _ => None
		}
	}
}


protected[main] class GComp extends GraphicsComponent {
	val vbo = new AVBO( SimpleAttributeProfile )
	val shader = ResourceManager.shader("shaders/Simple")
	val textureBlock = new TextureBlock(2048,2048)

	CommonRendering.quad(vbo,Vec3f.Zero,Vec3f.UnitX,Vec3f.UnitY,Vec2f(3.0f),Color.White,textureBlock( image("ui/checkbox_on.png")))

	def draw(graphicsContext: RenderingContext): Unit = {
//		GL11.glLoadIdentity()
//		GLU.gluLookAt(0,0,-10,0,0,0,0,1,0)

		GL.glSetState(GL11.GL_CULL_FACE,enable = false)

		shader.bind()
		textureBlock.bind()

		vbo.solidify()
		vbo.drawElements(GL11.GL_TRIANGLES)
	}
	def setPointOfView(pov: TCamera): Unit = {}
	protected def update(f: Float): Unit = {

	}
}