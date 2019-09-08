package arx.tyche.game.data

import java.util.concurrent.atomic.AtomicInteger

import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.{GameEntity, TGameEntity}
import arx.tyche.core.GlobeCoord
import arx.tyche.core.data.TyAuxData
import scalaxy.loops._

class Globe extends TyAuxData {
	var patches = Vector[TGameEntity]()
	var patchData = Vector[Patch]()
	var revision = new AtomicInteger(1)
	var delaunyTris = Vector[Vector[GlobeCoord]]()

	def markModified(): Unit = {
		revision.incrementAndGet()
	}
}

class Patch extends TGameEntityAuxData {
	var center = GlobeCoord()
	var neighbors = List[TGameEntity]()
	var vertices = List[GlobeCoord]()
	var terrainType : TerrainType = TerrainType.Desolation
	var hydration : Int = 0
	var entity : TGameEntity = GameEntity.Sentinel
	var sourceType : SourceType = SourceType.None

	def patchesWithinRange(r : Int) : Traversable[TGameEntity] = {
		var result = Set(entity)
		var previousRing = result
		for (i <- 0 until r optimized) {
			var currentRing = previousRing.flatMap(p => p[Patch].neighbors) -- result
			result ++= currentRing
			previousRing = currentRing
		}
		result
	}
}
object Patch {
	val MaxHydration = 4
}