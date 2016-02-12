package arx.tmp.game.logic.entities.data

import arx.core.representation.ConfigValue

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/14
 * Time: 7:24 AM
 */

protected[data] trait TConfigurableAuxData {
	def createFromSML ( conf : ConfigValue ) : Option[TAuxData]
}

object ConfigurableAuxData {
	val allSmlAuxData = ReflectionAssistant.instancesOfSubtypesOf[TConfigurableAuxData]
	val smlEntityAuxData = allSmlAuxData.collect { case a : TGameEntityAuxData => a }
}