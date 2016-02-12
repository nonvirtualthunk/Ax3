package arx.axistential.graphics.graphicsinfo

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 12:35 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.MaterialFlag
import arx.core.GeneralityLevel
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.graphics.Image
import arx.resource.ResourceManager

case class MaterialGraphicsInfo( textures : Array[Image] , color : ReadVec4f ) extends GameEntityGraphicsInfo {
	def icon: Image = textures(0)

	var mirror = false
	var spreadAND = 0xffff
	var spreadShift = 0
	var spreadMult = 1.0f
	var spread = false
}

object MaterialGraphicsInfoProvider extends TGameEntityGraphicsStructor {
	val graphicsInfoForMaterial = memoize( createGraphicsInfoForMaterial _ )

	def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Inclusive

	override def graphicsInfoFor(gameArchetype: GameArchetype): Option[GameEntityGraphicsInfo] = {
		gameArchetype match {
			case mat : Material => graphicsInfoForMaterial(mat)
			case other => None
		}
	}

	
	protected def imageArrayFromConfigValue ( value : ConfigValue , root : String ) = {
		if ( value.isArr ) {
			value.arr.map( s => image(root + s.str) ).toArray
		} else {
			Array.fill(6)( image(root + value.str) )
		}
	}

	def createGraphicsInfoForMaterial ( material : Material ) = {
		Material.allSML.get(material.baseName) match {
			case None => None
			case Some(configValue) => {
				val root = ConfigValue.extractFieldRecursive(configValue,"resourceRoot").strOrElse("")
				val textures =
					if ( configValue.hasField("texture") ) {
						Array.fill(6)( image(root + configValue.texture.str) )
					} else if ( configValue.hasField("textures") ) {
						val tx = configValue.textures
						if ( tx.isArr ) {
							imageArrayFromConfigValue(tx,root)
						} else {
							val txo = tx
							var ret : Array[Image] = null
							val sortedFields = txo.fields.toList.sortBy( _._1.count( c => c == '&') * -1 )
							for ( (field,value) <- sortedFields ) {
								val flags = field.split('&')
								if ( flags.forall( flag => material.flags.contains( MaterialFlag(flag) ) ) ) {
									ret = imageArrayFromConfigValue(value,root)
								}
							}
							if ( ret == null ) {
								if ( txo.hasField("normal") ) {
									ret = imageArrayFromConfigValue(txo.normal,root)
								} else {
									ret = Array.fill(6)(ResourceManager.defaultImage)
								}
							}
							ret
						}
					} else {
						Array.fill(6)(ResourceManager.defaultImage)
					}
				
				val color = configValue.color.v4OrElse(Vec4f.One)

				val mgi = MaterialGraphicsInfo(textures,color)
				if ( configValue.hasField("textureRepeat") ) {
					val repeat = configValue.textureRepeat.int
					mgi.spread = true
					mgi.spreadShift = (math.log(repeat) / math.log(2)).toInt
					mgi.spreadAND = (1 << mgi.spreadShift) - 1
					mgi.spreadMult = 1.0f / (1 << mgi.spreadShift).toFloat
				}
				Some(mgi)
			}
		}
	}
}