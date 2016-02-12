package arx.tmp.game.logic.entities.data

import arx.core.representation.ConfigValue


trait TConfigurableGameEntityAuxData extends TGameEntityAuxData with TConfigurableAuxData {
	def createFromSML ( conf : ConfigValue ) : Option[TGameEntityAuxData]
}