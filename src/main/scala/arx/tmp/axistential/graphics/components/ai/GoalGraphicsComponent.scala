package arx.axistential.graphics.components.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/13
 * Time: 10:49 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.ExamineGoal
import arx.axistential.ai.Goal
import arx.axistential.ai.TAIAgent
import arx.axistential.ai.TAIGroup
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.DesignationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.BuildVoxelGoal
import arx.axistential.graphics.helpers.IndividualVoxelRenderer
import arx.axistential.graphics.shader.GameUIShader
import arx.core.units.UnitOfTime
import arx.core.vec.Vec2f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.graphics.Image
import arx.graphics.TextureBlock
import org.lwjgl.opengl.GL11

trait TGoalGraphicsComponent extends GraphicsComponent {
	def renderGoal ( bucket : RenderBucket, goal : Goal )
}

class GoalGraphicsComponent extends ShinDynamicGraphicsComponent with TGoalGraphicsComponent {
	lazy val shader = GameUIShader(world,graphicsEngine)

	lazy val renderers = createRenderers()

	def createRenderers () = {
		val rs = ReflectionAssistant.instancesOfSubtypesOf[GoalRenderer[_]].groupBy( _.goalClass ).mapValues(_.head)
		rs.values.foreach( r => r.graphicsEngine = graphicsEngine )
		rs
	}

	def renderGoal(bucket: RenderBucket, goal: Goal): Unit = {
		renderers.getOrElse(goal.getClass,DefaultGoalRenderer).renderGoalUntyped(bucket,world,goal,None,isLeaf = false)
	}

	def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
		bucket.textureBlock.magFilter = GL11.GL_NEAREST
		bucket.drawOrder = GraphicsComponentDrawOrder.Last
		for ( group <- world.entitiesOfType[TAIGroup] ){
			for ( goalList <- List(group.goals,group.stagingGoals) ; goal <- goalList ) {
				renderers.getOrElse(goal.getClass,DefaultGoalRenderer).renderGoalUntyped(bucket,world,goal,None,isLeaf = false)
			}
		}
		for ( agent <- world.entitiesOfType[TAIAgent] ) {
			agent.activeLeafGoal match {
				case Some(leafGoal) => {
					val agentToPass = agent match {
						case physAgent : TPhysicalEntity with TAIAgent => Some(physAgent)
						case _ => Noto.warn("Non physical ai agent in goal rendering, can't be used, should investigate");None
					}
					renderers.getOrElse(leafGoal.getClass,DefaultGoalRenderer).renderGoalUntyped(bucket,world,leafGoal,agentToPass,isLeaf = true)
					var tmp = leafGoal.parentGoal
					while ( tmp.nonEmpty ) {
						renderers.getOrElse(tmp.get.getClass,DefaultGoalRenderer).renderGoalUntyped(bucket,world,tmp.get,agentToPass,isLeaf = true)
						tmp = tmp.get.parentGoal
					}
				}
				case _ => //do nothing
			}
		}
	}

	override def bucketIdentifier: Symbol = 'ui
	def bucketRequirements: RenderBucketRequirements = RenderBucketRequirements(UIAttributeProfile,shader).withIntIndices
}



abstract class GoalRenderer[T <: Goal : Manifest] {
	var graphicsEngine : GraphicsEngine = null

	def goalClass = manifest[T].runtimeClass

	protected[ai] def renderGoalUntyped(bucket: RenderBucket, world: World, goal: Goal, agent: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean){
		renderGoal(bucket: RenderBucket, world: World, goal.asInstanceOf[T], agent: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean)
	}
	def renderGoal(bucket: RenderBucket, world: World, goal: T, agent: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean)
}

class BuildVoxelGoalRenderer extends GoalRenderer[BuildVoxelGoal] {
	lazy val graphicsProvider = pio[TGameEntityGraphicsInfoProvider]
	val voxRenderers = memoize( (world:World,textureBlock:TextureBlock) => {
		new IndividualVoxelRenderer(textureBlock,world)
		.withExposedOnly(eo = true)
		.withExposureGrids(world.aux[TerrainData].materialGrid,world.aux[DesignationData].designations)
	})
	
	def renderGoal(bucket: RenderBucket, world: World, goal: BuildVoxelGoal, agent: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean): Unit = {
		val voxRenderer = voxRenderers(world,bucket.textureBlock)
		val mat = Material.allArchetypes.find( mat => goal.material.matchesEntity(mat.exampleBlock) ).getOrElse(Material.Sentinel)

		val DD = world.aux[DesignationData]
		val TD = world.aux[TerrainData]
//		val grid = new HashVoxelByteStore(0.toByte)
//		goal.voxels.foreach( v => grid(v) = 1.toByte )

		// we need an explicit voxel store because if this is a hypothetical goal it does not yet exist in the world
		val voxelStore = goal.voxels.asVoxelView

		val claimedColor = Vec4f(1.0f,1.0f,1.0f,0.85f)
		val unclaimedColor = Vec4f(0.9f,0.9f,0.9f,0.75f)
		val transform = new Transforms

		voxRenderer.withExposureGrids(voxelStore,world.aux[TerrainData].materialGrid,world.aux[DesignationData].designations)
		goal.voxels.edgeVoxels.foreachUnsafe( vox => {
			if ( ! TD.isOccupied(vox) ) {
				val mixer = if ( agent.isEmpty ) { 0.0f } else { CommonRendering.halfPulse(vox.x,vox.y,0.05f) }
				val color = mix(unclaimedColor,claimedColor,mixer)
				transform.colorMultiplier = color

				voxRenderer.drawVoxel(bucket,vox,mat,transform)
			}
		} )
//		true

//		CommonRendering.drawVoxels(bucket,goal.voxels,Some(grid),color,bucket(texture),repeat,exposedOnly = true)
	}
}
object DefaultGoalRenderer extends GoalRenderer[Goal]{
	def renderGoal(bucket: RenderBucket, world: World, goal: Goal, agent: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean): Unit = {}
}

class ExamineGoalRenderer extends GoalRenderer[ExamineGoal] {
	val bubble = image("axis/ui/goal/speech/thought_bubble.png")
	val icon = image("axis/ui/goal/speech/exclamation_red.png")
	val exclamationBubble = Image.composite(bubble,icon)

	override def renderGoal(bucket: RenderBucket, world: World, goal: ExamineGoal, agentOpt: Option[TPhysicalEntity with TAIAgent], isLeaf: Boolean): Unit = {
		for (agent <- agentOpt) {


			val pos = agent.headPosition.plusZ(0.5f)

			CommonRendering.billboardQuad(bucket,pos,Vec2f.One,Vec4f.One,exclamationBubble)
//			CommonRendering.billboardQuad(bucket,pos,Vec2f.One,Vec4f.One,icon)
		}
	}
}