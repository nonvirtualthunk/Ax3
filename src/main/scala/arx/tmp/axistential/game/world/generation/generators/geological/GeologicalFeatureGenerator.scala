package arx.axistential.game.world.generation.generators.geological

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/7/13
 * Time: 9:04 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.GeologicalFeatureData
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.world.SpatialRegion

class GeologicalFeatureGenerator extends TWorldGenerator {
	name = "Geological feature generator"
	var possibleFeatures = ReflectionAssistant.allSubTypesOf(classOf[GeologicalFeature]).asInstanceOf[List[Class[_ <: GeologicalFeature]]]//Vector(classOf[FractalMountain])

	//val geologicalRegionsByCell = new collection.mutable.HashMap[Int,List[GeologicalRegion]]()


	/**
	 * Perform this component's generation, storing the results in the provided world.
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		if ( phase == WorldGenerator.Geological ) {
			if ( stage == WorldGenerator.Heightmap ) {
				Noto.debug("Got to the geological bit of heightmap processing")
				val R = randomGenerator
				val featureData = world.auxData[GeologicalFeatureData]
				val step = featureData.step
				var openCells = for ( x <- 0 until world.dimensions.x by step ; y <- 0 until world.dimensions.y by step ) yield {
					Vec2i(x,y) + world.worldRegionAsSpatialRegion.lowerCorner.xy
				}
				Noto.debug("Open cells size : " + openCells.size)
				var addedFeatures = List[GeologicalFeature]()
				while ( openCells.nonEmpty ) {
					val idx = rand(R,0,openCells.size)
					val cell = openCells(idx)
					openCells = openCells.filterNot( _ == cell )
					val xy = cell + (step / 2)
					val featurePoint = Vec3i(xy.x,xy.y,0)

					val shouldHaveFeature = rand(R,0.0f,1.0f) > 0.5f
					var chosenFeature : Option[GeologicalFeature] = None
					if ( shouldHaveFeature ) {
						var probableFeatures = possibleFeatures
						while ( probableFeatures.nonEmpty && chosenFeature.isEmpty ) {
							val fidx = rand(R,0,probableFeatures.size)
							val featureClass = probableFeatures(fidx)
							probableFeatures = probableFeatures filterNot ( _ == featureClass )
							val feature = featureClass.getConstructor(classOf[World]).newInstance(world)
							if ( feature.canBePlacedAt(featurePoint,world,featureData) ) {
								chosenFeature = Some(feature)
							}
						}
					}

					chosenFeature match {
						case Some(feature) => {
							feature.placeAt(featurePoint,world,featureData)
							addedFeatures ::= feature
						}
						case None => //continue onward
					}
				}
				addedFeatures.ofType[TWorldGenerator]
			} else {
				Nil
			}
		} else {
			Nil
		}
	}
}

class GeologicalFeature(world:World) extends Serializable {
	var location : Vec3i = Vec3i(0,0,0)
	var extents : ReadVec3i = Vec3i(64,64,64)
	def halfExtents = extents / 2

	def canBePlacedAt(v : Vec3i,env : World ,gfd : GeologicalFeatureData) : Boolean = {
		var good = env.worldRegionAsSpatialRegion.containsXY( SpatialRegion(v,extents) )
		for ( x <- v.x - halfExtents.x until v.x + halfExtents.x ; y <- v.y - halfExtents.y until v.y + halfExtents.y ; if ( good ) ) {
			if ( gfd.geologicalFeatures(x,y) != null ) { good = false }
		}
		good
	}
	def placeAt ( v : Vec3i , env : World , gfd : GeologicalFeatureData ) {
		gfd.allFeatures ::= this
		location = v;
		for ( x <- v.x - halfExtents.x until v.x + halfExtents.x ; y <- v.y - halfExtents.y until v.y + halfExtents.y ) {
			gfd.geologicalFeatures(x,y) = this
		}
	}
}
