package arx.tmp.game.logic.entities.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/21/13
 * Time: 2:05 PM
 * Created by nonvirtualthunk
 */

import arx.core.units.MeasurementUnit

trait TFacet {
	def name : String

	override def hashCode = name.hashCode
	override def equals ( other : Any ) = {
		other match {
			case f : TFacet => f.name == this.name
			case _ => false
		}
	}
}

trait TOrderableFacet extends TFacet

case class OrdinalFacet (name : String,possibleValues : List[Any],possibleValueStrings : List[String]) extends TOrderableFacet
case class EnumeratedFacet (name : String,possibleValues : List[Any],possibleValueStrings : List[String]) extends TFacet
case class BitFlagFacet (name : String,bitPatternToString : Map[Int,String]) extends TFacet
case class ContinuousFacet (name : String,unit : Option[MeasurementUnit],order : Int = 1) extends TFacet with TOrderableFacet
case class UnorderedFacet (name : String) extends TFacet
