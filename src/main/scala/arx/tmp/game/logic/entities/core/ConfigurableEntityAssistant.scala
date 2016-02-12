package arx.tmp.game.logic.entities.core

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/14
 * Time: 7:25 AM
 */

import arx.Prelude.CaseInsensitiveString
import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.resource.ResourceManager

object ConfigurableEntityAssistant {
	def loadAllConfigsByPackage (baseResource : String, leafFieldName : String) = {
		val topLevelSML = ResourceManager.smlAll(baseResource)
		val packages = topLevelSML.fields.map(tup => new ConfigPackage(tup._1,tup._2.field(leafFieldName).fields))
		packages.flatMap( pack => {
			pack.children.map {
				case (k,v) => new CaseInsensitiveString (k) -> v
			}
		} ).toMap
	}


	def parseAllChildrenOfSMLAs[T <: TConfigurable : Manifest] (sml : ConfigValue) : List[T] = {

		def instantiate (s : ConfigValue) : T = {
			val constr = try {
				manifest[T].runtimeClass.getConstructor(classOf[ConfigValue])
			} catch {
				case e : Exception => null
			}
			if (constr != null) {
				constr.newInstance(s).asInstanceOf[T]
			} else {
				val t = ReflectionAssistant.instantiate(manifest[T])
				t.setFromSML(s,overwrite = true)
				t
			}
		}
		var ret = List[T]()
		if (sml.isEmpty) {
			// do nothing, but that's ok (we assume)
		} else if (sml.isArr) {
			for (s <- sml.arr) {
				val t = instantiate(s)
				ret ::= t
			}
		} else if (sml.isObj) {
			for ((k,v) <- sml.fields) {
				val t = instantiate(v)
				if (ReflectionAssistant.hasField(t,"name")) {
					ReflectionAssistant.setFieldValue(t,"name",k)
				}
				ret ::= t
			}
		} else {
			Noto.warn("parseAllChildrenAs(...) called with non-array, non-object sml")
		}
		ret
	}
}
