package arx.tmp.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/14
 * Time: 7:28 AM
 */

import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.entities.data.EntityFlag

object ConfiguredDescriptor {
	def apply(obj : ConfigValue) = {
		if (obj.isArr) {
			CompoundDescriptor(obj.arr.toList.map(c => new ConfiguredDescriptor(c)))
		} else {
			new ConfiguredDescriptor(obj) // this is essentially a lazy descriptor, to prevent lookup issues
		}
	}

	def fromString (str : String) = parseStr(str)

	protected def fromSML ( value : ConfigValue ) : TDescriptor = {
		if ( value.isObj ) {
			parseSML(value)
		} else {
			value.unwrapped match {
				case str : String => parseStr(str)
				case _ => {
					Noto.warn("internal value was neither string nor object, cannot parse to a descriptor")
					AnyEntityDescriptor
				}
			}
		}
	}

	protected def parseSML(obj: ConfigValue): TDescriptor = {
		val kindStr = obj.kind.str.toLowerCase

		// if the kindStr is of the form SomeIdentifier(arguments)
		if (kindStr.contains("(")) {
			return parseStr(kindStr)
		}

		for (parser <- descriptorParsers if parser.kindStringsLowerCaseSet.contains(kindStr)) {
			parser.parseFromSML(obj) match {
				case Some(desc) => return desc
				case None => // do nothing
			}
		}

		kindStr match {
			case "itemwithflag" => {
				var flags = Set[EntityFlag]()
				for ( flagValue <- extractSingularOrPlural(obj,"flag","flags") ) {
					EntityFlag.fromStringOpt(flagValue.str.toLowerCase) match {
						case Some(f) => flags += f
						case None => Noto.warn(s"ItemWithFlag descriptor in sml contained invalid entity flag ${flagValue.str}")
					}
				}
				new EntityWithFlagDescriptor(flags)
			}
			case _ => Noto.warn(f"could not parse descriptor from obj :\n$obj"); TDescriptor.Sentinel
		}

	}

	//+====================+ Helpers +====================+

	protected class StringCondition (prefix : String,check : (String) => Boolean, descrFunc : (String) => TDescriptor) {
		def apply (str : String) = {
			stringToDescriptor(str,prefix,check,descrFunc)
		}
	}

	protected def stringToDescriptor (str : String, prefix : String, check : (String) => Boolean, descrFunc : (String) => TDescriptor ) = {
		if (str.toLowerCase.startsWith (prefix.toLowerCase)) {
			val startIndex = prefix.length + 1
			val endIndex = math.max (startIndex, str.lastIndexOf (")"))
			val arg = str.substring (startIndex, endIndex)
			if (check (arg)) {
				Some (descrFunc (arg))
			} else {
				None
			}
		} else {
			None
		}
	}

	protected val descriptorParsers = ReflectionAssistant.instancesOfSubtypesOf[TConfiguredDescriptorParser]


	protected def parseStr(descriptorStr: String): TDescriptor = {
		for (parser <- descriptorParsers ; prefix <- parser.kindStringsLowerCaseSet) {
			stringToDescriptor(descriptorStr,prefix,_ => true,parser.parseFromString) match {
				case Some(desc) => return desc
				case None =>
			}
		}

		Noto.warn(s"Attempt to parse string '$descriptorStr' into descriptor failed, not recognized")
		AnyEntityDescriptor
	}
	//+====================+ /Helpers +====================+
}

/**
 * This is essentially a lazily parsed descriptor. We delay parsing as long as possible because
 * much relies on lookups, often by name, often of things that only exist in other config files.
 * Therefore, those configs must necessarily have been loaded in for us to be able to identify
 * if the descriptor is valid. For example: if the descriptor is "ItemOfType(Book)" we should be
 * able to identify whether or not such an archetype exists anywhere, and warn if it does not.
 * For context sensitive conveniences it becomes even more important. If the descriptor is just
 * "book", we need to be able to determine whether that is the name of a material, a material type,
 * an item archetype, etc, in order to definitively identify the kind of descriptor it is.
 */
@SerialVersionUID(1L)
class ConfiguredDescriptor(conf:ConfigValue) extends TDescriptor {
	lazy val intern = ConfiguredDescriptor.fromSML(conf)

	override def name: String = intern.name

	override def unambiguousDescriptor: Boolean = intern.unambiguousDescriptor

	override protected def doesMatch(ref: Any): Boolean = intern.matches(ref)

	override def exampleMatch: Serializable = intern.exampleMatch
}