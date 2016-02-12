package arx.axistential.graphics.components.renderers

import arx.application.Noto
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.TerrainData
import arx.axistential.graphics.graphicsinfo.MaterialGraphicsInfo
import arx.axistential.graphics.graphicsinfo.TCoveringGraphicsInfoProvider
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.graphics.TextureBlock
import arx.resource.ResourceManager

import scala.collection.mutable
import scalaxy.loops._

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/31/14
 * Time: 8:47 AM
 */


/** Tex coords are in q, material, k ordering*/
case class EnvironmentDrawingContext(
	texCoords:Array[Array[Array[ReadVec2f]]],
	texDims:Array[Array[Float]],
	materialTextureInfo:Array[MaterialGraphicsInfo],
	coveringOffset:Int,
	sidingMults:Array[Float]
)

object EnvironmentDrawingContext {
	val GraphicsInfoProvider = ReflectionAssistant.provideInstanceOf[TGameEntityGraphicsInfoProvider]
	val CoveringInfoProvider = ReflectionAssistant.provideInstanceOf[TCoveringGraphicsInfoProvider]

	var drawingContexts = new mutable.HashMap[(World,TextureBlock,Int),EnvironmentDrawingContext]()
	
//	def apply (texCoords:Array[Array[Array[ReadVec2f]]],texDims:Array[Array[Float]],materialTextureInfo:Array[MaterialGraphicsInfo],
//				  allMaterials:Array[Material],coveringOffset:Int,sidingMults:Array[Float]) = {
//		new EnvironmentDrawingContext(texCoords,texDims,materialTextureInfo,allMaterials,coveringOffset,sidingMults)
//	}
	
	def fromWorld (world : World,textureBlock : TextureBlock) = {
		val TD = world.aux[TerrainData]
		drawingContexts.getOrElseUpdate((world,textureBlock,TD._materialMapping.size + TD._coveringTypes.size),subBuildDrawingContext(world,textureBlock))
	}

	protected def subBuildDrawingContext (world : World,textureBlock : TextureBlock) = {
		val TD = world.aux[TerrainData]
		//We add on a buffer of two materials to the end, to allow for *less* clumsy handling of materials being added mid-render
		val allThings = (TD._materialMapping.backward.toList :::
			TD._coveringTypes.backward.toList :::
			TD._materialMapping.backward.toList.take(2)
			).toArray


		val matProvider = GraphicsInfoProvider
		val matInfo = allThings.map ( thing => {
			val info = thing match {
				case mat : Material => matProvider.graphicsInfoFor(mat)
				case cov : Covering => CoveringInfoProvider.graphicsInfoFor(cov)
			}

			info match {
				case mgi : MaterialGraphicsInfo => mgi
				case _ => {
					if ( thing.notSentinel ) {
						Noto.warn("Material had non MaterialGraphicsInfo gi")
					}
					MaterialGraphicsInfo( Array.fill(6)(ResourceManager.defaultImage) , Vec4f.One )
				}
			}
		})

		val tc = generateTexCoords(textureBlock,matInfo)
		val tdims = Array.ofDim[Float](6,allThings.size)
		for (i <- 0 until 6 optimized) {
			for ( j <- 0 until tc(i).length optimized) {
				tdims(i)(j) = tc(i)(j)(2).x - tc(i)(j)(0).x
			}
		}


		val coveringOffset = TD._materialMapping.size
		val sidingMults = (for ( q <- 0 until 6 ) yield { q match {
			case Top => 1.0f
			case Bottom => 0.9f
			case Left | Right => 0.96f
			case _ => 0.93f
		} }).toArray

		EnvironmentDrawingContext(tc,tdims,matInfo,coveringOffset,sidingMults)
	}

	def generateTexCoords(textureBlock: TextureBlock, matInfo : Array[MaterialGraphicsInfo]) = {
		// q : mat : k
		val baseTexCoords : Array[Array[Array[ReadVec2f]]] = textureBlock.synchronized {
			(matInfo.map {
				case MaterialGraphicsInfo(textures,color) => {
					textures.map( textureBlock.getOrElseUpdate )
				}
			}).toArray
		}

		val texCoords = Array.ofDim[Array[Array[ReadVec2f]]](6)
		for ( q <- 0 until 6 ) {
			texCoords(q) = Array.ofDim[Array[ReadVec2f]](matInfo.size)
			for ( matIndex <- 0 until matInfo.size ) {
				texCoords(q)(matIndex) = baseTexCoords(matIndex)(q)
			}
		}

		texCoords
	}
}
