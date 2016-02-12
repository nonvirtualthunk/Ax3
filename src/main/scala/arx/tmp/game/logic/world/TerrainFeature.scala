package arx.tmp.game.logic.world

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/11/11
 * Time: 5:49 PM
 * Created by nonvirtualthunk
 */

object TerrainFeature{
	val Biome = 10
	val Geological = 30;
	val Hydrological = 50;
	val Botanical = 70;
	val Cultural = 90;

	val FeatureTypeNames = Map( Biome -> "biome" , Geological -> "geological" , Hydrological -> "hydrological" , Botanical -> "botanical" , Cultural -> "cultural" )
	val FeatureTypes = List(Biome,Geological,Hydrological,Botanical,Cultural)
}

abstract class TerrainFeature(world: World){
	var bounds = new SpatialRegionGroup()
	var featureType = TerrainFeature.Geological

	var alterationsByPhase = Map[String,(World,SpatialRegion) => Unit]()
	var completionByPhase = Map[String,Boolean]()

	def bounded: Boolean = bounds.regions.nonEmpty
}