package arx.axistential.game.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/29/13
 * Time: 11:56 AM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.GranularData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.data.world.WindData
import arx.axistential.testbed.thermo.logic.ThermodynamicData
import arx.core.gen.ArxGenerators._
import arx.core.gen.SimplexNoise
import arx.core.units.UnitOfDistance
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.datastructures.TaleaGrid
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TSanityCheckable.Assert

import scalaxy.loops._

class WeatherComponent extends GameEngineComponent {
	dependencies ::= classOf[HeightmapComponent]

	lazy val clouds = world.createEntityTypeQuery[Cloud]

	override def initialize(): Unit = {
		val numClusters = 3
		val numCloudPoints = 8

		val cloudGen = Scale(0.085f) >> Fractal(3)(Simplex(new SimplexNoise(System.currentTimeMillis()))) >> Mult(20)
		val reg = world.worldRegionAsSpatialRegion

		val prevailingTheta = world.aux[WindData].prevailingWindDirection
		val baseVector = Vec2f(cosf(prevailingTheta),sinf(prevailingTheta))
		val orthoVector = Vec2f(cosf(prevailingTheta + pi * 0.5f),sinf(prevailingTheta + pi * 0.5f))

		val basePoint = Vec2f(VoxelCoord.Center.x,VoxelCoord.Center.y) - Vec2f(baseVector * reg.dimensions.x * 0.5f) - Vec2f(orthoVector * reg.dimensions.x * 0.5f)

		for ( clusterIndex <- 0 until numClusters ) {
			val startY = (0.1f + 0.8f * (clusterIndex/numClusters.toFloat)) * reg.dimensions.y
			val yDim = (0.8f * (1.0f/numClusters)) * reg.dimensions.y
			val newClouds = for ( i <- 0 until numCloudPoints ) yield {
				val point = basePoint + baseVector * cloudGen(i) + orthoVector * (startY + yDim * (i/numCloudPoints.toFloat))
				val cloud = new Cloud

				cloud.position = point
				cloud.basePrecipitation = 10.0f
				cloud.currentPrecipitation = cloud.basePrecipitation
				cloud.radius = (reg.dimensions.y.voxels * 0.5f) / numCloudPoints.toFloat

				world.addEntity(cloud)
				cloud
			}

			for ( cloud <- newClouds ) {
				for ( nearbyCloud <- newClouds.filterNot( c => c == cloud || distance(c.position,cloud.position) > 250.0f ) ) {
					val dist = distance(nearbyCloud.position,cloud.position)
					cloud.connectedTo ::= CloudConnection(nearbyCloud, dist.voxels , 1.0f - dist / 250f)
				}
			}
		}
	}


	var tillNext = 0.07.seconds
	def update(time: UnitOfTime): Unit = {
		tillNext -= time
		val doPrecipitation = tillNext <= zeroSeconds
		if ( doPrecipitation ) { tillNext = 0.1.seconds }

		var modifiedVoxels = Set[VoxelCoord]()
		
		val WD = world.aux[WindData]
		val GD = world.aux[GranularData]
		val TD = world.aux[TerrainData]
		val TPD = world.aux[TopologicalData]
		val ThermD = world.aux[ThermodynamicData]

		for ( cloud <- clouds ) {
			val speed = cloud.baseSpeed.inVoxelsPerSecond * time.inSeconds
			val direction = WD.windVector(cloud.position.x.toInt,cloud.position.y.toInt)
			cloud.position += direction * speed


			if ( doPrecipitation ) {
				modifiedVoxels ++= precipitate(cloud,GD,TD,TPD,ThermD)
			}
		}
		
		val taleaPositions = modifiedVoxels.map( v => (v >> Talea.dimensionPo2) << Talea.dimensionPo2 )
		val taleae = taleaPositions.map( v => GD.levelGrid.taleaFor(v.x,v.y,v.z) )

		GD.levelGrid.fireEvent( TaleaModificationsCompletedEvent(taleae) )

		for ( cloud <- clouds ) {
			var tmp = cloud.position
			for ( connection <- cloud.connectedTo ) {
				val delta = connection.cloud.position - tmp
				val deltaLength = delta.lengthSafe
				val deltaNormalized = delta.normalize

				val scaleTo = (deltaLength - connection.equilibriumDistance.inVoxels) * 0.003f * connection.connectionStrength

				tmp += deltaNormalized * scaleTo * (time.inSeconds / 0.0166667f)
			}

			cloud.position = tmp
		}
	}

	def precipitate ( cloud : Cloud , GD : GranularData, TD : TerrainData, topology : TopologicalData, therm : ThermodynamicData ) = {
		var bounces = 30
		var ret = Set[VoxelCoord]()
		for ( i <- 0 until 15 optimized ) {
			var found = false
			while ( ! found && bounces > 0 ) {
				val theta = rand(0.0f,2.0f * pi)
				val rad = rand(0.0f,cloud.radius.inVoxels * 0.5f) + rand(0.0f,cloud.radius.inVoxels * 0.5f)
				var v : VoxelCoord = VoxelCoord(Vec2i(cloud.position),VoxelCoord.Center.z) + Vec3i((cosf(theta) * rad).toInt,(sinf(theta) * rad).toInt,0)
//				v = TD.firstSolidInColumn(v,world.worldRegion.dimensions.z/2).plusZ(1) //The open above the solid block
				val h = topology.heightmap(v)
				v = if ( ! world.worldRegionAsSpatialRegion.contains(v) || h + VoxelCoord.Center.z <= world.worldRegionAsSpatialRegion.minZ ) {
					VoxelCoord.Sentinel
				} else {
					VoxelCoord(v.x,v.y,VoxelCoord.Center.z + h)
				}

				if ( v.notSentinel ) {
					val cur = GD.levelGrid(v)
					if ( cur < 60 ) {
						val temp = therm.temperatureAt(v.x,v.y,v.z)

						if ( temp == 0 ) {
						
							GD.levelGrid(v) = (cur + 15).max(60).toByte
							if ( cur == 0 ) {
								GD.materialGrid(v) = TD.materialMapping(Material.withName("snow"))
							}

							ret += v
						}
						found = true
					} else { bounces -= 1 }
				} else {
					bounces -= 1
				}
			}
		}
		ret
	}
}

class Cloud extends GameEntity {
	var position : ReadVec2f = Vec2f.Zero
	var baseSpeed = 1.m_s
	var basePrecipitation : Float = 0.0f
	var currentPrecipitation : Float = 0.0f
	var radius = zeroMeters
	var connectedTo = List[CloudConnection]()

	def voxelX = position.x.toInt
	def voxelY = position.y.toInt

	override def sanityChecks: List[Assert] =
		Assert(position != Vec2f.Zero,"No position assigned") ::
		Assert(radius > zeroMeters,"Zero radius") ::
		super.sanityChecks
}
case class CloudConnection ( cloud : Cloud , equilibriumDistance : UnitOfDistance, connectionStrength : Float )
