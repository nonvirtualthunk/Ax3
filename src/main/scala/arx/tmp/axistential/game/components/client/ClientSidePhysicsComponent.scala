package arx.axistential.game.components.client

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/1/15
 * Time: 8:35 PM
 */

import arx.axistential.game.components.client.ClientSidePhysicsComponent.PositionRecord
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.networking.communicators.PhysicsNetworkingCommunicator
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.ObjectCoord
import arx.tmp.game.networking.data.NetworkingWorldData
import com.carrotsearch.hppc.LongObjectOpenHashMap

class ClientSidePhysicsComponent extends GameEngineComponent {
	lazy val physicalEntities = world.createEntityTypeQuery[TPhysicalEntity]
//	val previousPositionsByEntity = new LongObjectOpenHashMap[]()
	val positionInterpolation = new LongObjectOpenHashMap[PositionRecord]()

	def positionRecordFor (ent : TPhysicalEntity) = {
		if (!positionInterpolation.containsKey(ent.uid)) {
			val newRecord = new PositionRecord(ent.position,ent.position)
			positionInterpolation.put(ent.uid, newRecord)
			newRecord
		} else {
			positionInterpolation.lget
		}
	}


	override def initialize(): Unit = {
		world.aux[NetworkingWorldData].customCommunicators += new PhysicsNetworkingCommunicator(physicalEntities,world)
	}

	override def update(time: UnitOfTime): Unit = {
		val ds = time.inSeconds
		for (ent <- physicalEntities) {
			val newPos = ent.position + ent.velocity.inVoxelsPerSecond * ds
			val record = positionRecordFor(ent)
			val interPos = record.interpolate(newPos)
			ent.position = interPos
			record.update(newPos)
		}
	}


}

object ClientSidePhysicsComponent {
//	class PositionRecord (previousPositions : List[ObjectCoord]) {
//		def extrapolate
//	}


	class PositionRecord (var nm1 : ObjectCoord, var nm2 : ObjectCoord) {
		def interpolate (cur : ObjectCoord) : ObjectCoord = {
			nm1 * 0.05f + nm2 * 0.15f + cur * 0.8f
		}
		def update (newVal : ObjectCoord): Unit = {
			nm2 = nm1
			nm1 = newVal
		}
	}
}