package arx.axistential.game.constants

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 12:41 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.application.Noto
import arx.core.traits.TArxEnum

class StateOfMatter(val name : String) extends TArxEnum {
	def key = name
}

object StateOfMatter {
	val Solid = new StateOfMatter("solid")
	val Liquid = new StateOfMatter("liquid")
	val Gas = new StateOfMatter("gas")

	val allStatesOfMatter = List(Solid,Liquid,Gas)

	def fromString ( str : String ) = allStatesOfMatter.find( _.name == str.toLowerCase ).getOrElse({
		Noto.warn(s"Invalid state of matter provided $str, returning solid instead")
		Solid
	})
}
