package arx.axistential.game.logic.requirements

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/14/13
 * Time: 12:39 PM
 */

import arx.axistential.game.archetypes.Material
import arx.axistential.game.entities.MaterialBlock
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity

case class SpecificMaterialDescriptor(material : Material) extends TEntityDescriptor {
	override def name = material.name
	def matchesEntity(gameEntity: GameEntity): Boolean = {
		gameEntity match {
			case mb : MaterialBlock => mb.material == material
			case _ => false
		}
	}

	override def exampleMatch: GameEntity = material.exampleBlock
}
