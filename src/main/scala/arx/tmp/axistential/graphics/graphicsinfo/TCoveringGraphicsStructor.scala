package arx.axistential.graphics.graphicsinfo

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/10/15
 * Time: 1:25 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.core.representation.ConfigValue

import scala.collection.mutable

trait TCoveringGraphicsStructor {
	def graphicsInfoFor (covering : Covering) : Option[MaterialGraphicsInfo]
}

class PlantCoveringGraphicsStructor extends TCoveringGraphicsStructor {
	def stateToName (state : Int) = {
		var ret = ""
		if (state.isBitSet(Covering.CroppedShort)) {
			ret += "cropped"
		}

		ret match {
			case "" => "base"
			case _ => ret
		}
	}

	lazy val gameEntityGraphicsProvider = pio[TGameEntityGraphicsInfoProvider]

	val cache = new mutable.HashMap[Covering,MaterialGraphicsInfo]

	override def graphicsInfoFor(covering: Covering): Option[MaterialGraphicsInfo] = covering.archetype match {
		case plantSpecies : PlantSpecies => {
			Some(cache.getOrElseUpdate(covering,computeInfoFor(plantSpecies,covering.state)))
		}
		case _ => None
	}

	def computeInfoFor(plantSpecies : PlantSpecies, state : Int) : MaterialGraphicsInfo = {
		PlantSpecies.allSML.get(plantSpecies.name) match {
			case Some(sml) => {
				if (sml.coveringDrawing.isEmpty) { Noto.warn(s"PlantCoveringGraphicsStructor encountered plant without a coveringDrawing section in sml: $plantSpecies"); CoveringGraphicsInfoProvider.SentinelGraphicsInfo}
				else {
					val root = ConfigValue.extractFieldRecursive(sml,"resourceRoot").strOrElse("")
					val CD = sml.coveringDrawing
					val stateSML = if (CD.states.hasField(stateToName(state))) { CD.states.field(stateToName(state)) } else { CD.states.base }

					val textures = stateSML.textures.arr.map(v => image(root + "/" + v.str)).toArray
					val color = stateSML.color.v4OrElse(Color.White)

					val mgi = MaterialGraphicsInfo(textures,color)
					if ( stateSML.hasField("textureRepeat") ) {
						val repeat = stateSML.textureRepeat.int
						mgi.spread = true
						mgi.spreadShift = (math.log(repeat) / math.log(2)).toInt
						mgi.spreadAND = (1 << mgi.spreadShift) - 1
						mgi.spreadMult = 1.0f / (1 << mgi.spreadShift).toFloat
					}
					mgi
				}
			}
			case None => Noto.warn("Plant covering graphics structor cannot create info from non-sml based plant"); CoveringGraphicsInfoProvider.SentinelGraphicsInfo
		}
	}
}

trait TCoveringGraphicsInfoProvider {
	def graphicsInfoFor (covering : Covering) : MaterialGraphicsInfo
}
object CoveringGraphicsInfoProvider extends TCoveringGraphicsInfoProvider {
	lazy val structores = ReflectionAssistant.instancesOfSubtypesOf[TCoveringGraphicsStructor]

	def graphicsInfoFor (covering : Covering) : MaterialGraphicsInfo = structores.findFirstWith(s => s.graphicsInfoFor(covering)) match {
		case Some(ginfo) => ginfo._2
		case None => SentinelGraphicsInfo
	}

	val SentinelGraphicsInfo = MaterialGraphicsInfo(Array.fill(6)(image("default/defaultium.png")),Color.White)
}