package arx.axistential.testbed.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/6/15
 * Time: 8:03 AM
 */

import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.helpers.FarmZoneData
import arx.axistential.game.entities.helpers.Zone
import arx.axistential.game.logic.ai.groupgoals.PlantFarmGoal
import arx.axistential.graphics.shader.GameUIShader
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

class FarmPlantingAllocationTestbed extends VisualizationDebugging {
	val zone = new Zone(VoxelRegion.fromCorners(VoxelCoord.Center - Vec3i(10,10,1),VoxelCoord.Center + Vec3i(16,16,10)))
	val goal = new PlantFarmGoal(zone)
	zone.aux[FarmZoneData].speciesToPlant = Some(PlantSpecies.withName("oak"))
	
	override def setUpGameEngine(): Unit = {
		gameEngine.world.addEntity(zone)
		val TD = gameEngine.world.aux[TerrainData]
		for (x <- -10 to 16; y <- -10 to 16) {
			TD.setMaterialAt(VoxelCoord.Center.plusX(x).plusY(y),Material.withName("soil"))
		}
		for (x <- -1 to 16; y <- -1 to 16) {
			TD.setMaterialAt(VoxelCoord.Center.plusX(x).plusY(y).plusZ(1),Material.withName("soil"))
		}
		for (x <- -1 to 1; y <- -1 to 1 ; z <- 2 to 5) {
			TD.setMaterialAt(VoxelCoord.Center.plusX(x).plusY(y).plusZ(z),Material.withName("soil"))
		}

		for (z <- 5 to 10) {
			TD.setMaterialAt(VoxelCoord.Center + Vec3i(-8,-8,z),Material.withName("soil"))
		}
		Timed {
			goal.recomputePlantingLocations()
		}.println("recompute locs:","s")
	}

	override def voxelColors: Map[ReadVec4f, List[VoxelCoord]] = {
		val TD = gameEngine.world.aux[TerrainData]
		val FD = zone.aux[FarmZoneData]
		var res = Map(ReadVec4f(1.0f,0.0f,0.0f,1.0f) -> List(VoxelCoord.Center))
		val dirtColor = Vec4f(0.8f,0.2f,0.2f,1.0f)
		val plantColor = Vec4f(0.2f,0.2f,0.8f,1.0f)
		zone.region.foreachUnsafe(v => {
			if (TD.isSolid(v)) {
				res += dirtColor -> (VoxelCoord(v) :: res.getOrElse(dirtColor,Nil))
			}
		})

		res += plantColor -> FD.plantAtLocations.map(_.toVoxelCoord)
		res
	}
}

abstract class VisualizationDebugging extends BareBonesTestbed {
	class TWRC extends ShinDynamicGraphicsComponent {
		override def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
			val tc = bucket.textureBlock(image("default/blankBordered.png"))
			for ((color,voxels) <- voxelColors ; voxel <- voxels) {
				CommonRendering.drawCube(bucket,voxel,Vec3f.One,color,tc)
			}
		}

		override def bucketIdentifier: Symbol = 'TWRC

		override def bucketRequirements = RenderBucketRequirements(UIAttributeProfile,GameUIShader(world,graphicsEngine))
	}

	override def setUpGameEngine(): Unit = {
		
	}

	override def setUpUI(): Unit = {
		
	}

	override def setUpGraphicsEngine(): Unit = {
		graphicsEngine addComponent(new TWRC)

		graphicsEngine.pov match {
			case anth: Camera =>
				anth.moveSpeed *= 0.25f
				anth.turnSpeed *= 0.5f
				anth.eye = ObjectCoord(-16.0f,-16.0f,17.0f)
				anth.angles = Vec2f(0.737f,0.737f)
		}
	}

	def voxelColors : Map[ReadVec4f,List[VoxelCoord]]
}