package arx.axistential.game.archetypes.item

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 8:52 AM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype.PhysicalArchetypedEntity
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.entities.traits.SimplePhysicalEntity
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.units.Dimensions
import arx.tmp.game.logic.entities.data.TInheritableAuxData

class SimpleItemArchetype protected () extends ItemArchetype with NonDiscoverable {
	var madeOf : List[MaterialComposition] = Nil
	var collisionShape = new CubeCollisionShape(new Dimensions(1.meter,1.meter,1.meter))
	override protected def createPhysicalInstance: PhysicalArchetypedEntity = {
		val ent = new SimplePhysicalEntity()
		ent.materialComposition = madeOf
		ent.collisionShape = collisionShape
		allAuxiliaryData.ofType[TInheritableAuxData].foreach(_.copyToInstance(ent))
		ent
	}
}

object SimpleItemArchetype {
	def apply(name : String,dims : Dimensions, material : Material, categories : Set[String], subtypeOf : Option[String]) = {
		val arch = new SimpleItemArchetype
		arch.name = name
		arch.collisionShape = new CubeCollisionShape(dims)
		arch.itemCategories = categories
		arch.subtypeOf = subtypeOf
		arch.madeOf = MaterialComposition(material,1.0f) :: Nil
		arch
	}
}
