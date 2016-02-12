package arx.axistential.game.archetypes.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/17/13
 * Time: 1:33 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype.PhysicalArchetypedEntity
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.tmp.game.logic.entities.core.TInstantiableArchetype

trait TPhysicalEntityArchetype extends TInstantiableArchetype {
	override def createInstance : PhysicalArchetypedEntity = super.createInstance.asInstanceOf[PhysicalArchetypedEntity]

	protected def createInstanceInternal: PhysicalArchetypedEntity = {
		createPhysicalInstance
	}

	protected def createPhysicalInstance : PhysicalArchetypedEntity
}

object TPhysicalEntityArchetype {
	type PhysicalArchetypedEntity = TPhysicalEntity
}