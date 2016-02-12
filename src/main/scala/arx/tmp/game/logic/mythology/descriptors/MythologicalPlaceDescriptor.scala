package arx.tmp.game.logic.mythology.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/15/14
 * Time: 7:39 AM
 */

import arx.tmp.game.logic.mythology.MythologicalEntityData

case class MythologicalPlaceDescriptor (positive : Boolean) extends TMythologicalEntityDescriptor {
	override def matchesMythologicalEntity(entity: MythologicalEntityData): Boolean = {
		entity.kind.toLowerCase match {
			case "place" => entity.flags.contains("positive") == positive
			case _ => false
		}
	}
}
