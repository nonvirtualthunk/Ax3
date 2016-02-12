package arx.axistential.graphics.components.ai.renderers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/4/14
 * Time: 2:14 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.TAIAgent
import arx.axistential.game.entities.helpers.ConstructionSite
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.CraftRecipeGoal
import arx.axistential.graphics.components.ai.GoalRenderer
import arx.core.vec.Vec4f
import arx.engine.world.World

class CraftRecipeGoalRenderer extends GoalRenderer[CraftRecipeGoal] {
	lazy val mainRenderer = pio[TGenericEntityRenderer]

	def renderGoal(renderTarget: RenderBucket, world: World, goal: CraftRecipeGoal, agent: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean): Unit = {
		val textureBlock = renderTarget.textureBlock
		if ( goal.hasFreeStandingOutput ) {

			goal.constructionSite match {
				case cs : ConstructionSite => {
					val subTransforms = new Transforms
					subTransforms.translation = cs.position
					subTransforms.zPercent = (cs.percentComplete * 20).toInt / 20.0f - 0.01f
					mainRenderer.renderEntity(cs.constructingEntity,world,renderTarget,textureBlock,subTransforms,graphicsEngine)

					val postTransforms = new Transforms
					postTransforms.colorMultiplier = Vec4f(1.0f,1.0f,1.0f,0.3f)
					postTransforms.translation = cs.position
					mainRenderer.renderEntity(cs.constructingEntity,world,renderTarget,textureBlock,postTransforms,graphicsEngine)
				}
				case _ => Noto.warn("Freestanding recipe output without a construction site...odd")
			}
		}
	}
}