package arx.axistential.graphics.graphicsinfo

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/13
 * Time: 10:38 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.Thinking
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.data.entity.CosmeticData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.logic.ai.goals.DigGoal
import arx.axistential.game.logic.ai.goals.MoveGoal
import arx.core.GeneralityLevel
import arx.core.datastructures.CountMap
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget
import arx.resource.ResourceManager

import scala.collection.mutable

object SpeciesGraphicsInfoProvider extends ConfiguredGraphicsInfoProvider[AnimalSpecies](AnimalSpecies.allSML _) {
//	override protected def subLoadGraphicsInfoFor(arch: Species): GameEntityGraphicsInfo = {
//		super.subLoadGraphicsInfoFor(arch) match {
//			case NoGraphicsInfo => { //couldn't load normally, check for special graphics info
//				Species.allSML.get(arch.name.toLowerCase) match {
//					case Some(sml) => {
//						sml.kind.strOrElse("none") match {
//							case "" => {
//
//							}
//							case _ => NoGraphicsInfo
//						}
//					}
//					case None => NoGraphicsInfo
//				}
//			}
//			case other => other
//		}
//	}
}


object CreatureGraphicsStructor extends ATypedGenericEntitySubRenderer[CreatureEntity] with TGameEntityGraphicsStructor {
	lazy val graphicsInfoProvider = pio[TGameEntityGraphicsInfoProvider]
	def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Limited

	val countersByCreature = new CountMap[CreatureEntity]()
	val whichByCreature = new mutable.HashMap[CreatureEntity,String]()
	val recentFlipsByEntity = new mutable.HashMap[GameEntity,Float]


	def intersectionShapeFor(ent: CreatureEntity, env: World): TIntersectable = new UprightBillboard(ent.position,ent.boundingDimensions.x.inVoxels,ent.boundingDimensions.z.inVoxels)

	def renderEntity(ent: CreatureEntity, env: World, renderTarget: TRenderTarget, textureBlock: TextureBlock, transforms: Transforms, graphicsEngine: GraphicsEngine): Unit = {
		val frame = frameFor(ent)

		val cosmeticData = ent.aux[CosmeticData]
		val color = frame.color * cosmeticData.color
		val pos = ent.fastFootPosition + transforms.translation
		val img = frame.image
		val dims = Vec3f(ent.boundingDimensions.x.inVoxels, ent.boundingDimensions.y.inVoxels, ent.boundingDimensions.z.inVoxels)

		val scale = img.aspectRatio

		val billboardWidth = dims.z * scale//math.max(dims.x,dims.y)
		val billboardHeight = dims.z

		val baseTexCoords = textureBlock.getOrElseUpdate( img )

		val zPercent = transforms.zPercent

		val toEyeVector = (ent.position - graphicsEngine.pov.eye).normalizeSafe
		val ortho = toEyeVector.cross(Vec3f.UnitZ)
		val invOrtho = ortho * -1.0f
		val facing = ent.facing
		val flip = ortho.dot(facing) < invOrtho.dot(facing)

		val texCoords = if (ent.alive) {
			if ( flip ) { Array( baseTexCoords(1) , baseTexCoords(0) , baseTexCoords(3) , baseTexCoords(2) ) } else { baseTexCoords }
		} else {
			textureBlock(image("axis/entities/animals/textures/animals/tombstone.png"))
		}

		val lighting = CommonRendering.LightResult(1.0f,1.0f,Vec3f(1.0f,1.0f,1.0f))
		if ( zPercent >= 1.0f ) {
			CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,lighting,texCoords)
		} else {
			val zStart = texCoords(0).y
			val zEnd = zStart + (texCoords(2).y - zStart) * zPercent
			val newTexCoords = Array(ReadVec2f(texCoords(0).x,zStart),ReadVec2f(texCoords(1).x,zStart),ReadVec2f(texCoords(2).x,zEnd),ReadVec2f(texCoords(3).x,zEnd))
			CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight * zPercent),color * transforms.colorMultiplier,lighting,newTexCoords)
		}
	}

	def renderHash(baseEnt: CreatureEntity, env: World, textureBlock: TextureBlock): Int = 0

	def frameFor ( ce : CreatureEntity ) : SpriteFrame = {
		graphicsInfoProvider.graphicsInfoFor(ce.species) match {
			case AnimatedBillboardGraphicsInfo(animations) => {
				val which = ce.activeLeafGoal.getOrElse( Thinking ) match {
					case MoveGoal(_,_,_) => {
						"walking"
					}
					case DigGoal(_) => {
						"working"
					}
					case _ => "default"
				}

				if ( whichByCreature.getOrElse(ce,"") != which ) {
					whichByCreature(ce) = which
					countersByCreature(ce) = -0.02f
				}

				var tmpCounter = countersByCreature(ce)

				if ( tmpCounter < 0.0f ) {
					countersByCreature(ce) = tmpCounter + 0.01666666f
					val tmpFrame = animations("default").frames.head
					tmpFrame
				} else {
					val animation = animations(which)
					val duration = animation.totalDuration

					var frames = animation.frames
					while ( frames.size > 1 && tmpCounter > frames.head.duration.inSeconds ) {
						tmpCounter -= frames.head.duration.inSeconds
						frames = frames.tail
					}

					var newCounter = countersByCreature(ce) + 0.016666666f
					while ( newCounter > duration.inSeconds ) { newCounter -= duration.inSeconds }
					countersByCreature(ce) = newCounter

					frames.head
				}
			}
			case bgi : BillboardGraphicsInfo => SpriteFrame(bgi.textures.head,bgi.color,1.second)
			case NoGraphicsInfo =>
				Noto.warn(s"no graphics info for species ${ce.species}")
				SpriteFrame( ResourceManager.defaultImage , Vec4f.One, 1.second )
		}
	}

	override def graphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = gameEntity match {
		case ce : CreatureEntity => Some( CustomRendererGraphicsInfo(this,frameFor(ce).image,dynamic = true) )
		case _ => None
	}
}