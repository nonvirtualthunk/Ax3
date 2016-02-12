package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/18/15
 * Time: 5:51 PM
 */

import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.TInstantiableArchetype

trait TConfigurableArchetype extends GameArchetype with TInstantiableArchetype with TConfigurable {

	/** Creates all applicable aux data from the provided sml and applies it to the given archetype */
	protected def applyConfigurableAuxData (fromSML : ConfigValue, toInst : GameArchetype ) {
		for ( ad <- ConfigurableAuxData.smlEntityAuxData.flatMap( d => d.createFromSML(fromSML) ) ) {
			toInst.manualAddAuxData(ad.asInstanceOf[TGameEntityAuxData])
		}
	}

}
