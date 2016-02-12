package arx.axistential.graphics.components.entities.customrenderers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/13
 * Time: 10:19 AM
 */

import arx.axistential.game.entities.CreatureEntity
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget

class CharacterRenderer extends TGenericEntitySubRenderer {
	def intersectionShapeFor(ent: GameEntity, env: World): Option[TIntersectable] = ent match {
		case ce : CreatureEntity =>
			Some( new UprightBillboard(ce.position,ce.boundingDimensions.x.inVoxels,ce.boundingDimensions.z.inVoxels) )
		case _ =>
			None
	}

	def renderEntity(baseEnt: GameEntity, env: World, renderTarget: TRenderTarget, textureBlock: TextureBlock, transforms: Transforms, graphicsEngine: GraphicsEngine): Unit = {
		baseEnt match {
			case ce : CreatureEntity => {

			}
			case _ =>
		}
	}

	def pointsRequiredFor(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Option[Int] = None

	/** See TGenericEntityRenderer */
	def renderHash(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Int = 0 //not needed because these guys are dynamic
}
