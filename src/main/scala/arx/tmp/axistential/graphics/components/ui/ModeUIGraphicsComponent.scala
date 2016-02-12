package arx.axistential.graphics.components.ui

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/13
 * Time: 11:55 AM
 */

import arx.axistential.graphics.components.ai.TGoalGraphicsComponent
import arx.axistential.graphics.shader.GameUIShader
import arx.axistential.ui.modes.TAxistentialGameMode

import scala.collection.mutable


class ModeUIGraphicsComponent(val modeStack : mutable.Stack[TGameMode]) extends DynamicGraphicsComponent{
	lazy val shader = GameUIShader(world,graphicsEngine)
	dependencies ::= classOf[TGoalGraphicsComponent]
	lazy val goalGraphics = reify[TGoalGraphicsComponent]

	protected def update(f: Float) {
		val bucket = getBucket

		if ( modeStack.nonEmpty ) {
			modeStack.top.drawUI(bucket)
			modeStack.top match {
				case axm : TAxistentialGameMode => {
					for ( goal <- axm.hypotheticalGoals ) { goalGraphics.renderGoal(bucket,goal) }
				}
				case _ =>
			}
		}
	}

	def bucketIdentifier = modeStack.top.customBucketIdentifier.getOrElse('uiBucket)
	def bucketRequirements = {
		modeStack.top.customRenderRequirements.getOrElse( RenderBucketRequirements(UIAttributeProfile,shader) )
	}
}