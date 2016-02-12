package arx.tmp.game.logic.descriptors

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/17/13
 * Time: 5:12 PM
 * Created by nonvirtualthunk
 */

import arx.core.Moddable
import arx.tmp.game.logic.entities.Player



class Selector(var inputDescriptions : Map[String,TDescriptor] ) {

}

class SelectorResult(var inputs : Map[String,Serializable], val selector : Selector) {
	def isPartial = ! isFull
	def isFull = inputs.size == selector.inputDescriptions.size

	def inputAs[T <: Any : Manifest](key:String) = resolveModdable(inputs(key)).asInstanceOf[T]
	def inputAsOpt[T <: Any : Manifest](key:String) = inputs.get(key).map( a => resolveModdable(a).asInstanceOf[T])

	def player(key : String) = inputAsOpt[Player](key).getOrElse(Player.Sentinel)

	def resolveModdable( a : Any ) = a match {
		case m : Moddable[_] => m.resolve()
		case other => other
	}

	def unselected = selector.inputDescriptions.filterNot( t => inputs.contains(t._1) )
//		if ( manifest[T].erasure.isAssignableFrom(inputs(key).getClass) ) {
//
//	} else {
//
//	}
}

object Selector {
	implicit def fromMap ( map : Map[String,TDescriptor] ) = new Selector(map)
	implicit def fromMapModdable ( map : Map[String,TDescriptor] ) = Moddable(new Selector(map))
	implicit def toMap ( selector : Selector ) = selector.inputDescriptions

	val Sentinel : Selector = new Selector(Map())
}

object SelectorResult {
//	implicit def fromMap ( map : Map[String,Any] ) = new SelectorResult(map)
//	implicit def toMap ( selector : SelectorResult ) = selector.inputs

	def apply ( map : Map[String,Serializable] , selector : Selector ) = new SelectorResult(map,selector)
}