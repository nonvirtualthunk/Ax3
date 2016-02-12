package arx.tmp.game.logic.entities.core

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/7/13
 * Time: 10:43 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.tmp.game.logic.entities.data.TInheritableAuxData
import arx.tmp.game.logic.entities.traits.TArchetypedEntity


trait TInstantiableArchetype extends GameArchetype {
	def createInstance : TArchetypedEntity = {
		val base = createInstanceInternal
		base.archetype = this
		base.name = this.instanceName
		for ( ad <- allAuxiliaryData.ofType[TInheritableAuxData] ) {
			ad.copyToInstance( base )
		}
		base
	}

	protected def createInstanceInternal : TArchetypedEntity

	lazy val exampleInstance = createInstance
}