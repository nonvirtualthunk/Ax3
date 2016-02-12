package arx.axistential.game.world.generation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 12:23 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.application.TLoggingLevelProvider
import arx.core.datastructures.CountMap
import arx.core.gen._
import arx.core.traits.ArxEnum
import arx.core.traits.TArxEnum
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.core.Dependency
import arx.core.TDependable
import arx.tmp.game.logic.world.SpatialRegion

import scala.collection.parallel.ExecutionContextTaskSupport
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.TaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Random

class WorldGenerator {
	def generateWorld ( world : World , description : WorldGenerationDescription) {
		timeAndPrint("World Generation") {
			world.worldRegionAsSpatialRegion = SpatialRegion( VoxelCoord.Center, description.dimensions )

			val timeTakenByGenerator = new CountMap[TWorldGenerator]

			var generators = description.generators
			generators = Dependency.recursiveResolveDependables(generators).ofType[TWorldGenerator]
			generators = Dependency.topologicalSort(generators)

			import WorldGenerator._
			val phases = List(Geological,Meteorological,Hydrological,Botanical,Zoological,Cultural)
			val stages = List(Pregen,Heightmap,Voxel,Entity)

			//The idea here is that we could later do background generation of worlds with only a single
			//thread, thereby preventing it from massively slowing down the main game
			val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(8))
			generators.foreach( _.taskSupport = taskSupport )

			for ( stage <- stages ; phase <- phases ){
				var generatorIndex = 0
				while ( generatorIndex < generators.size ) {
					val gen = generators(generatorIndex)
					val startTime = System.currentTimeMillis()
					val newGenerators = gen.generate(world,stage,phase,new Progress)
					val endTime = System.currentTimeMillis()

					timeTakenByGenerator.increment(gen,(endTime - startTime) * 0.001f)

					generators = generators ::: newGenerators

					generatorIndex += 1
				}
			}

			Noto.fine(WorldGeneratorPrintLevel,"+====================+ World Gen Breakdown +====================+ ")
			timeTakenByGenerator.intern.foreach{ case (gen,time) => Noto.fine(WorldGeneratorPrintLevel,s"\t${gen.name} : ${time}s") }
			Noto.fine(WorldGeneratorPrintLevel,"+====================+                     +====================+ ")
		}
	}
}

object WorldGeneratorPrintLevel extends TLoggingLevelProvider {
	override def loggingLevel: Int = Noto.Fine
}

object WorldGenerator {
	class Phase(name : String) extends ArxEnum(name)

	val Geological = new Phase("Geological")
	val Meteorological = new Phase("Meteorological")
	val Hydrological = new Phase("Hydrological")
	val Botanical = new Phase("Botanical")
	val Zoological = new Phase("Zoological")
	val Cultural = new Phase("Cultural")


	class Stage(name : String) extends TArxEnum { def key = name }
	val Pregen = new Stage("Pregen")
	val Heightmap = new Stage("Heightmap")
	val Voxel = new Stage("Voxel")
	val Entity = new Stage("Entity")
}

class Progress {
	var percentDone = 0.0f
}

trait TWorldGenerator extends TDependable {
	var name = this.getClass.getSimpleName.fromCamelCase

	// These generators will be replaced with ones that use a specific seed by the world creation infrastructure
	var simplexGenerator : SimplexNoise = new SimplexNoise(1L)
	var randomGenerator = new Random(1L)

	var taskSupport : TaskSupport = new ExecutionContextTaskSupport()

	/**
	 * Perform this component's generation, storing the results in the provided world.
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate ( world : World , stage : WorldGenerator.Stage , phase : WorldGenerator.Phase , progress : Progress ) : List[TWorldGenerator]
}

class WorldGenerationDescription ( val generators : List[TWorldGenerator] , val dimensions : Vec3i, seed : Long = System.currentTimeMillis() ) extends Serializable {
	generators.foreach( generator => {
		generator.simplexGenerator = new SimplexNoise(seed)
		generator.randomGenerator = new Random(seed)
	})
}