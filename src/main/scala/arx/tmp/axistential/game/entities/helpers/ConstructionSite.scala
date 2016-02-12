package arx.axistential.game.entities.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/31/13
 * Time: 3:24 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.GeneralityLevel
import arx.core.vec.Vec4f
import arx.core.vec.coordinates.ObjectCoord
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget

class ConstructionSite( val constructingEntity : GameEntity , var percentComplete : Float = 0.0f ) extends TPhysicalEntity {
	constructingEntity match {
		case pe : TPhysicalEntity => {
			this.collisionShape = pe.collisionShape
		}
		case _ => Noto.warn("Why are you constructing a non physical entity?")
	}

	this.aux[InventoryData].storageLimit = None
}

object ConstructionSite extends TGameEntityGraphicsStructor with TGenericEntitySubRenderer {
	lazy val provider = pio[TGameEntityGraphicsInfoProvider]
	lazy val mainRenderer = pio[TGenericEntityRenderer]

	def apply ( position : ObjectCoord , constructing : GameEntity ) = {
		val cs = new ConstructionSite(constructing)
		cs.position = position
		cs
	}

	override def graphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = {
//		gameEntity match {
//			case cs : ConstructionSite => {
//				val base = provider.graphicsInfoFor(cs.constructingEntity)
//				Some( CustomRendererGraphicsInfo(this,base.icon,dynamic = true) )
//
////				val transforms = new Transforms
//////				transforms.colorMultiplier = Vec4f(1.0f,1.0f,1.0f,0.3f)
//////				transforms.maxZ = cs.constructingEntity match {
//////					case pe : TPhysicalEntity => cs.position.z - pe.boundingDimensions.z.inVoxels * (0.5f - cs.percentComplete) + 0.05f
//////					case _ => 1000.0f
//////				}
////				transforms.zPercent = cs.percentComplete
////				Some(TransformedGraphicsInfo(base,transforms))
//			}
//			case _ => None
//		}
		None
	}


	def intersectionShapeFor(ent: GameEntity, env: World): Option[TIntersectable] = None

	def renderEntity(baseEnt: GameEntity, world: World, renderTarget: TRenderTarget, textureBlock: TextureBlock, transforms: Transforms, graphicsEngine: GraphicsEngine): Unit = {
		val cs = baseEnt.asInstanceOf[ConstructionSite]

		val subTransforms = transforms.copy
		subTransforms.translation = cs.position
		subTransforms.zPercent = cs.percentComplete
		mainRenderer.renderEntity(cs.constructingEntity,world,renderTarget,textureBlock,subTransforms,graphicsEngine)

		val postTransforms = transforms.copy
		postTransforms.colorMultiplier = Vec4f(1.0f,1.0f,1.0f,0.3f)
		postTransforms.translation = cs.position
		mainRenderer.renderEntity(cs.constructingEntity,world,renderTarget,textureBlock,postTransforms,graphicsEngine)
	}

	def pointsRequiredFor(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Option[Int] = None

	/** See TGenericEntityRenderer */
	def renderHash(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Int = baseEnt match {
		case cs : ConstructionSite => (cs.percentComplete * 1000).toInt
		case _ => 0
	}

	def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Specific
}
