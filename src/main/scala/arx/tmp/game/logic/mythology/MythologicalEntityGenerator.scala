package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/15/14
 * Time: 7:43 AM
 */

import arx.tmp.game.logic.descriptors.TDescriptor

object MythologicalEntityGenerator {
	val subGenerators = ReflectionAssistant.instancesOfSubtypesOf[TMythologicalEntitySubGenerator]

	def generateMythologicalEntity (descriptor : TDescriptor, forPantheon : Pantheon) : MythologicalEntity = {
		forPantheon.mythologicalEntities.filter( descriptor.matches ) match {
			case s if s.nonEmpty => MathPrelude.randFrom(s)
			case _ => {
				val newEnt = subGenerators.findFirstWith( _.generateEntityWithDescriptor(descriptor,forPantheon) ) match {
					case Some(ent) => ent._2
					case None => new MythologicalEntity
				}
				forPantheon.mythologicalEntities += newEnt
				newEnt
			}
		}
	}
}
