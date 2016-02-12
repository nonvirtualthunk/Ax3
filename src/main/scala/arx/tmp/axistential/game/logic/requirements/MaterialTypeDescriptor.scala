package arx.axistential.game.logic.requirements

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/16/13
 * Time: 9:52 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.application.Noto
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.MaterialType
import arx.axistential.game.entities.MaterialBlock
import arx.core.datastructures.OneOrMore
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.descriptors.TConfiguredDescriptorParser
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity

class MaterialTypeDescriptor ( requiredFlags : Set[MaterialType] ) extends TEntityDescriptor {
	override def name = if ( requiredFlags.isEmpty ) { "" } else { requiredFlags.map(_.name).reduceLeft(_ + " " + _) }
	def this ( flags : OneOrMore[MaterialType] ) { this(flags.toSet) }

	def matchesMaterial ( material : Material ) = requiredFlags subsetOf material.materialTypes

	def matchesEntity(gameEntity: GameEntity): Boolean = gameEntity match {
		case mb : MaterialBlock => matchesMaterial(mb.material)
		case _ => false
	}

	lazy val exampleMaterial = Material.allArchetypes.find( matchesMaterial ) match {
		case Some(mat) => new MaterialBlock(mat)
		case None =>
			Noto.warn(s"No material block with the appropriate types $requiredFlags")
			new MaterialBlock( Material.Sentinel )
	}
	override def exampleMatch: GameEntity = exampleMaterial





}

object MaterialTypeDescriptor extends TConfiguredDescriptorParser {
	val Metallic = new MaterialTypeDescriptor( Material.Metallic )
	val Woody = new MaterialTypeDescriptor( Material.Wood )

	/**
	 * Given a full config value that might represent a descriptor that this can
	 * parse, attempt to create a descriptor, if valid, or return None otherwise.
	 */
	override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = {
		if (sml.`type`.nonEmptyValue || sml.types.nonEmptyValue) {
			val types = extractSingularOrPlural(sml,"type","types").map(v => MaterialType(v.str)).toSet
			Some(new MaterialTypeDescriptor(types))
		} else {
			None
		}
	}

	/**
	 * If this can be parsed from a string of the form: "DescriptorIdentifier(argument)",
	 * this function should return the string identifier to indicate that this parser
	 * should be used. This should also be the value used in the
	 * "kind" field if represented as an object.
	 */
	override def kindStrings: List[String] = "MaterialWithType" :: "MaterialType" :: Nil

	/**
	 * Given the argument from a string of the form: "DescriptorIdentifier(argument)",
	 * create a new descriptor.
	 */
	override def parseFromString(arg: String): TDescriptor = new MaterialTypeDescriptor(MaterialType(arg))
}