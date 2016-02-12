package arx.axistential.graphics.components.general

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/12
 * Time: 2:34 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.axistential.graphics.components.TCoreEnvironmentViewerComponent
import arx.axistential.graphics.components.renderers.TSubVoxelEnvironmentRenderer
import arx.core.units.UnitOfTime
import arx.graphics.DefaultAttributeProfile

class VoxelAnimationGraphicsComponent extends DynamicGraphicsComponent with TVoxelAnimationGraphicsComponent {
	dependencies ::= classOf[TCoreEnvironmentViewerComponent]
	lazy val coreEnvironmentViewerComp = reify[TCoreEnvironmentViewerComponent]

	val renderer = provideInstanceOf[TSubVoxelEnvironmentRenderer]
	var animations = Set[VoxelAnimation]()

	/**
	 * Instantiates a voxel animation and includes it in the set of
	 * active animations
	 * @param timeout length of time to keep the animation active, a negative
	 *                timeout will keep the animation active indefinitely, until
	 *                explicitly returned
	 * @return the <code>VoxelAnimation</code> instance
	 */
	def requestVoxelAnimation (timeout : UnitOfTime): VoxelAnimation = {
		val animation = new VoxelAnimation(timeout)
		animations += animation
		animation
	}
	def returnVoxelAnimation (animation : VoxelAnimation) {
		animations -= animation
	}

	protected def update(f: Float) {
		val secondsElapsed = (f * 0.016666667f * gameEngine.timescale).seconds
		val bucket = getBucket
		for ( animation <- animations ) {
			animation.age += secondsElapsed
			if ( animation.timeout >= zeroSeconds && animation.age > animation.timeout ) { animation.finished = true }

			if ( ! animation.finished ) {
				renderer.renderSubVoxels(bucket,bucket.textureBlock,gameEngine.environment,animation.voxels)
			}
		}
		animations = animations.filterNot ( _.finished )
	}

	def bucketIdentifier = 'voxelAnimation
	def bucketRequirements =
		RenderBucketRequirements(
			DefaultAttributeProfile,
			coreEnvironmentViewerComp.shader _,
			coreEnvironmentViewerComp.textureBlock
		)

	override def drawOrder = GraphicsComponentDrawOrder.First
}

trait TVoxelAnimationGraphicsComponent {
	def requestVoxelAnimation (timeout : UnitOfTime): VoxelAnimation
	def returnVoxelAnimation (animation : VoxelAnimation)
}