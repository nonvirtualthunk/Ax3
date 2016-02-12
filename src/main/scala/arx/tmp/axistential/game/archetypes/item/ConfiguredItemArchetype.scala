package arx.axistential.game.archetypes.item

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 8:51 AM
 */

import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.traits.TConfigurablePhysicalEntityArchetype
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.entities.traits.SimplePhysicalEntity
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.representation.ConfigValue
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.entities.data._

class ConfiguredItemArchetype ( initialSML : ConfigValue ) extends ItemArchetype
	with TConfigurableArchetype
	with TConfigurablePhysicalEntityArchetype
{
	var sml = initialSML
	setFromSML(initialSML)

	def setFromSML ( sml : ConfigValue, overwrite : Boolean ) {
		this.sml = sml
		name = sml.name.strOrElse(name)
		description = sml.description.strOrElse(description)
		subtypeOf = if (sml.subtypeOf.isEmpty) {subtypeOf} else {Some(sml.subtypeOf.str)}
		for (cat <- extractSingularOrPlural(sml,"itemCategory","itemCategories")) {
			itemCategories += cat.str
		}

		freeStanding = sml.freeStanding.boolOrElse(freeStanding)
		if ( sml.hasField("baseValue") ) {
			baseValue = sml.field("baseValue").intOrElse(baseValue)
		}

		_auxData.clear()
		applyConfigurableAuxData(sml,this)
	}

	protected def createPhysicalInstance: TPhysicalEntityArchetype.PhysicalArchetypedEntity = {
		val isLightSource = sml.hasField("lightStrength")

		val inst = if ( isLightSource ) {
			val tmp = new TPhysicalEntity with TLightSource {
				def lightLocation: VoxelCoord = physData.position.toVoxelCoord
				lightStrength = sml.lightStrength.int.toByte
				lightColor = sml.lightColor.v4OrElse(lightColor)
			}
			tmp
		} else { new SimplePhysicalEntity }
		val kind = sml.kind.strOrElse("item")

		val PD = inst.physData
		PD.collisionShape = parseCollisionShape(sml,"box")
		PD.dynamic = true

		if ( kind == "stockpile" ) {
			val sourceInventory = sml.sourceInventory.boolOrElse(orElse = true)

			val ID = inst.auxData[InventoryData]
			ID.isSourceInventory = sourceInventory
			if ( sml.storageLimit.nonEmptyValue ) { ID.storageLimit = Some(sml.storageLimit.int) }
		}

		applyConfigurableAuxData(sml,this)


		if ( sml.hasField("material") ) {
			inst.materialComposition = MaterialComposition( Material.withName(sml.material.str), 1.0f ) :: Nil
		}

		inst
	}

	protected def readResolve : Object = ItemArchetype.archetypeWithName(this.name)
	private def writeObject (stream : ObjectOutputStream) { stream.writeUTF(name) }
	private def readObject (stream : ObjectInputStream) { name = stream.readUTF() }

}