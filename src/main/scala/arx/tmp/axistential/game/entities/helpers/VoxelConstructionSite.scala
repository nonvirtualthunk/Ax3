package arx.axistential.game.entities.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/15/13
 * Time: 9:42 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.MaterialFlag
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.axistential.graphics.graphicsinfo.MaterialGraphicsInfo
import arx.axistential.graphics.helpers.IndividualVoxelRenderer
import arx.core.GeneralityLevel
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.SpatialRegion
import arx.graphics.traits.TRenderTarget
import arx.graphics.Image
import arx.graphics.TextureBlock

class VoxelConstructionSite(var voxels : List[VoxelCoord],materialDescriptor : TEntityDescriptor) extends TPhysicalEntity {
	name = "Voxel Construction Site " + voxels.headOption.getOrElse(VoxelCoord.Sentinel)
	protected val exampleMaterial = materialDescriptor.exampleMatch match {
		case pe : TPhysicalEntity => pe.primaryMaterial.withFlag(MaterialFlag.Constructed)
		case _ => Material.Sentinel
	}

	var pcntComplete = Map[VoxelCoord,Float]().withDefaultValue(0.0f)
	var materialFor = Map[VoxelCoord,Material]().withDefaultValue(exampleMaterial)
	val inventory = this.aux[InventoryData]
	inventory.isSourceInventory = false
	inventory.storageLimit = None

	this.dynamic = true
	this.ghost = true

	val region = SpatialRegion.fromPoints(voxels)
	this.position = VoxelCoord(region.center).toObjectCoord

	this.collisionShape = new CubeCollisionShape(region.dimensions.x.voxels x region.dimensions.y.voxels x region.dimensions.z.voxels)
}

object VoxelConstructionSite extends TGameEntityGraphicsStructor with TGenericEntitySubRenderer {
	lazy val provider = pio[TGameEntityGraphicsInfoProvider]
	val voxRenderers = memoize( (world:World,textureBlock:TextureBlock) => {
		new IndividualVoxelRenderer(textureBlock,world)
			.withExposedOnly(eo = true)
			.withExposureGrids(world.aux[TerrainData].materialGrid)
	})

	def apply ( position : ObjectCoord , voxels : List[VoxelCoord], materialDescriptor : TEntityDescriptor) = {
		val cs = new VoxelConstructionSite(voxels,materialDescriptor)
		cs.position = position
		cs
	}

	override def graphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = {
		gameEntity match {
			case cs : VoxelConstructionSite => Some(CustomRendererGraphicsInfo(this,Image.Sentinel,true))
			case _ => None
		}
	}

	def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Specific

	def intersectionShapeFor(ent: GameEntity, env: World): Option[TIntersectable] = {
		None
	}

	def renderEntity(baseEnt: GameEntity, env: World, renderTarget: TRenderTarget, textureBlock: TextureBlock, transforms: Transforms, graphicsEngine: GraphicsEngine): Unit = {
		val cs = baseEnt.asInstanceOf[VoxelConstructionSite]

		val renderer = voxRenderers(env,textureBlock).withExposureGrids()

		for ( vox <- cs.voxels ) {
			val pcnt = cs.pcntComplete(vox)
			if (pcnt > 0.0f) {
				val mat = cs.materialFor(vox)
				provider.graphicsInfoFor(mat) match {
					case MaterialGraphicsInfo(textures,color) => {
						val tc = textureBlock(textures(0))
						//			CommonRendering.partialUprightBillboardQuad(renderTarget,vox.toObjectCoordFoot,Vec2f.One,Vec4f.One,tc,0.0f,0.1f + pcnt * 0.9f)
//						CommonRendering.drawCube(renderTarget,vox.toObjectCoordFoot.plusZ(0.5f * pcnt),Vec3f(1.0f,1.0f,pcnt),Vec4f.One,tc)
						val tr = new Transforms
						tr.zPercent = pcnt
						renderer.drawVoxel(renderTarget,vox,mat,tr)
					}
					case _ => Noto.warn("Material with non-material graphics info encountered during voxel construction site rendering")
				}
			}
		}
	}

	def pointsRequiredFor(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Option[Int] = None
	/** See TGenericEntityRenderer */
	def renderHash(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Int = 0
}
