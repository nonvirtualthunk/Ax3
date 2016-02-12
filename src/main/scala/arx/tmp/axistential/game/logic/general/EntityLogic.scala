package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/24/13
 * Time: 1:31 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype.PhysicalArchetypedEntity
import arx.axistential.game.data.entity.animal.AnimalData
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.entity.NoInventory
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.traits.SimplePhysicalEntity
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.GeneralityLevel
import arx.core.GeneralityLevel.GeneralityLevel
import arx.core.introspection.CopyAssistant
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget

object EntityLogic {
	def destroyEntity ( entity : GameEntity ) {
		if (entity.inWorld) {
			entity match {
				case phys : TPhysicalEntity => {
					val ID = phys.auxDataOrElse[InventoryData](NoInventory)

					ID.allHeldEntities.foreach( ent => {
						ent.heldBy = None
						ent.position = phys.position
					} )

					for (holder <- phys.heldBy) {
						val holderInv = holder.auxDataOrElse[InventoryData](NoInventory)
						holderInv.removeHeldEntity(phys)
					}
				}
				case _ => Noto.warn(s"Don't know how to destroy non physical entity $entity")
			}

			for (creature <- entity.ifIs[CreatureEntity]) {
				creature.die()

				val corpseArch = CorpseArchetype.forSpecies(creature.species)
				val corpse = corpseArch.createInstance
				corpse.manualAddAuxData( CopyAssistant.copyShallow(creature.aux[AnimalData]) )
				// treat all living thing parts that we grabbed as being non-structural, the corpse
				// can lose any part of it without ceasing to be a corpse
				corpse.aux[AnimalData].parts = corpse.aux[AnimalData].parts.map(p => p.copy(structural = false))
				corpse.position = creature.position
				corpse.dynamic = true
				corpse.ghost = true

				entity.world.addEntity(corpse)
			}


			entity.world.removeEntity(entity)
		}
	}
}

class CorpseArchetype protected (forSpecies : AnimalSpecies) extends ItemArchetype {
	name = forSpecies.name + " corpse"
	override protected def createPhysicalInstance: PhysicalArchetypedEntity = {
		val corpse = new CorpseEntity
		val mat = Material.withName("flesh")
		corpse.materialComposition = List(MaterialComposition(mat,1.0f))
		val inst = forSpecies.exampleInstance.expectAs[TPhysicalEntity].getOrElse(TPhysicalEntity.Sentinel)
		corpse.collisionShape = new CubeCollisionShape(inst.boundingDimensions)

		corpse
	}
}

class CorpseEntity extends SimplePhysicalEntity

object CorpseArchetype extends ATypedGenericEntitySubRenderer[TPhysicalEntity] with TGameEntityGraphicsStructor {
	val forSpecies = memoize( (species : AnimalSpecies) => {
		new CorpseArchetype(species)
	})

	override def intersectionShapeFor(ent: PhysicalArchetypedEntity, env: World): TIntersectable = new UprightBillboard(ent.position,ent.boundingDimensions.x.inVoxels,ent.boundingDimensions.z.inVoxels)
	override def renderHash(baseEnt: PhysicalArchetypedEntity, env: World, textureBlock: TextureBlock): Int = 0
	override def renderEntity(ent: PhysicalArchetypedEntity, env: World, renderTarget: TRenderTarget, textureBlock: TextureBlock, transforms: Transforms, graphicsEngine: GraphicsEngine): Unit = {
		for (arch <- ent.archetype.ifIs[CorpseArchetype]) {
			val pos = ent.fastFootPosition + transforms.translation
			val dims = ent.boundingDimensions.inVoxels
			val img = image("axis/entities/animals/textures/animals/tombstone.png")
			val scale = img.aspectRatio
			val billboardWidth = dims.z * scale//math.max(dims.x,dims.y)
			val billboardHeight = dims.z
			val color = Vec4f.One
			val lighting = CommonRendering.LightResult(1.0f,1.0f,Vec3f.One)
			val texCoords = textureBlock(img)

			CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,lighting,texCoords)
		}
	}
	override def generalityLevel: GeneralityLevel = GeneralityLevel.Specific

	override def graphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = {
		gameEntity.archetype match {
			case ca : CorpseArchetype =>
				val img = image("axis/entities/animals/textures/animals/tombstone.png")
				Option (BillboardGraphicsInfo (img :: Nil, Vec4f.One, selfIlluminated = false))
			case _ => None
		}
	}
}