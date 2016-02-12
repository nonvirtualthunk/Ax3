package arx.tmp.game.logic.entities.core

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/14/14
 * Time: 8:12 AM
 */

import arx.Prelude._
import arx.core.representation.ConfigValue

import scala.collection.mutable

object ConfigurableGameArchetype {

	def organizeByName[T <: GameArchetype] ( archetypes : Traversable[T] ) = {
		val ret = new mutable.HashMap[CaseInsensitiveString,T]()
		for ( arch <- archetypes ) { ret(arch.name.toLowerCase) = arch }
		ret
	}
}

class ConfigPackage (val name : String, val children : Map[String,ConfigValue]) {

}