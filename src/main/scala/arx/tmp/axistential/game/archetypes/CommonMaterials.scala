package arx.axistential.game.archetypes

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/13
 * Time: 9:59 AM
 * To change this template use File | Settings | File Templates.
 */


object CommonMaterials {
	lazy val Stone = Material.withName("stone")
	lazy val ConstructedStone = Material.withName("stone").withFlag(Material.Constructed)
	lazy val SmoothedStone = Material.withName("stone").withFlag(Material.Smoothed)

	lazy val Wood = Material.materialWithName("wood")
}
