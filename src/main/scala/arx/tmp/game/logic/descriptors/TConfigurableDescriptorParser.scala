package arx.tmp.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/12/14
 * Time: 8:58 AM
 */

import arx.core.representation.ConfigValue

trait TConfiguredDescriptorParser {
	/**
	 * Given a full config value that might represent a descriptor that this can
	 * parse, attempt to create a descriptor, if valid, or return None otherwise.
	 */
	def parseFromSML (sml : ConfigValue) : Option[TDescriptor]

	/**
	 * If this can be parsed from a string of the form: "DescriptorIdentifier(argument)",
	 * this function should return the string identifier to indicate that this parser
	 * should be used. This should also be the value used in the
	 * "kind" field if represented as an object.
	 */
	def kindStrings : List[String]

	/**
	 * Given the argument from a string of the form: "DescriptorIdentifier(argument)",
	 * create a new descriptor.
	 */
	def parseFromString (arg : String) : TDescriptor

	val kindStringsLowerCaseSet = kindStrings.map(_.toLowerCase).toSet
}