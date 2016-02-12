package arx.axistential.testbed.ai.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/8/14
 * Time: 1:41 PM
 */

import arx.axistential.game.archetypes.CommonMaterials
import arx.axistential.game.archetypes.Material
import arx.axistential.game.components.physics.BulletPhysicsComponent
import arx.axistential.game.components.physics.DebugData
import arx.axistential.game.components.physics.VoxelDebugGraphicsComponent
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f

class ConstructionBBT extends BareBonesTestbed {
	def setUpGameEngine(): Unit = {
		val world = gameEngine.activeWorld

		world.aux[TerrainData]._setFromFunction( (x,y,z) => {
			if ( z < VoxelCoord.Center.z ) {
				CommonMaterials.SmoothedStone
			} else {
				Material.Sentinel
			}
		} , 30)

		gameEngine.addComponent( new LightingComponent )
		//		gameEngine.addComponent( new PhysicsComponent )
		gameEngine.addComponent( new BulletPhysicsComponent )
	}

	def setUpGraphicsEngine(): Unit = {
		gameEngine.world.aux[DebugData].active = true

		graphicsEngine.addComponent( new EnvironmentViewerComponent2 )
		graphicsEngine.addComponent( new PhysicalEntityViewerComponent )
		graphicsEngine.addComponent( new ModeUIGraphicsComponent(gameController.modeStack,"axistential") )
		graphicsEngine.addComponent( new VoxelDebugGraphicsComponent )

		val camera = new AnthologiconCamera(Vec3f(-60,0.0f,5.0f))
		camera.useGlobalUp = true
		graphicsEngine.pov = camera

	}

	def setUpUI(): Unit = {
		gameController.addTopLevelMode(new ConstructionGameMode)
	}

	override def update(f: Float): Unit = {
		super.update (f)
	}

	protected class ConstructionGameMode extends TGameMode {


		override def drawUI(bucket: RenderBucket): Unit = {
			val (start,end) = currentWorldMouseLine
			graphicsEngine.intersectWorld( start,end ) match {
				case Some(in) =>
					val v = in.voxel + dirvec(in.side)
					CommonRendering.drawCube(bucket,v,Vec3f.One,Vec4f.One,bucket(image("axis/entities/materials/textures/dirt.png")))
				case None =>
			}
		}

		onEvent {
			case WorldMousePressEvent(button,start,end,modifiers) => {
				graphicsEngine.intersectWorld( start,end ) match {
					case Some(in) =>
						val v = in.voxel + dirvec(in.side)
						val TD = world.aux[TerrainData]
						val b = TD.materialMapping( Material.withName("stone") )
						TD.materialGrid.setAndNotify(v,b)
					case None =>
				}
			}
		}
	}
}


