package arx.axistential.game.networking.communicators

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/1/15
 * Time: 8:11 PM
 */

import arx.Prelude._
import arx.axistential.game.entities.traits.PhysicalEntityData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.query.ContinuousQuery
import arx.core.units.Velocity
import arx.core.vec.ReadVec3f
import arx.core.vec.coordinates.ObjectCoord
import arx.engine.world.World
import arx.tmp.game.networking.data.TCustomNetworkCommunicator
import com.carrotsearch.hppc.LongObjectOpenHashMap
import com.esotericsoftware.minlog.Log

class PhysicsNetworkingCommunicator(physEntities : ContinuousQuery[TPhysicalEntity], world : World) extends TCustomNetworkCommunicator {
	var lastPosition = new LongObjectOpenHashMap[ObjectCoord]
	
	override def messagesToSend(): Seq[AnyRef] = {
		var ret = Seq[EntityPositionUpdate]()
		for (ent <- physEntities) {
			// entities that are held by others don't need position updates to be sent
			if (ent.heldBy.isEmpty) {
				if (ent.dynamic) {
					// dynamic entities get an update if they have moved since the last time
					val lastPos = if (! lastPosition.containsKey(ent.uid)) {
						lastPosition.put(ent.uid,ObjectCoord.Sentinel)
						ObjectCoord.Sentinel
					} else {
						lastPosition.lget()
					}
					
					if (lastPos != ent.position) {
						ret :+= EntityPositionUpdate(ent.uid,ent.position,ent.velocity.inMetersPerSecond)
						lastPosition.put(ent.uid,ent.position)
					}
				}
			}
		}
		if (ret.nonEmpty){
			Seq(EntityPositionMessage(ret))
		} else { Nil }
	}

	override def messageReceived(msg: Any): Boolean = msg match {
		case EntityPositionMessage(updates) =>
			for (EntityPositionUpdate(uid,pos,vel) <- updates) {
				if (world.entitiesByUID.containsKey(uid)) {
					val ent = world.entitiesByUID.get(uid)
					val PD = ent.aux[PhysicalEntityData]

					PD.velocity = Velocity(vel.x.m_s,vel.y.m_s,vel.z.m_s)
					val newPos = pos + PD.velocity.inVoxelsPerSecond * 0.005f
					PD.position = newPos
				} else { Log.error(s"Received position update for non-present entity, uid : $uid") }
			}
			true
		case _ => false
	}
}

case class EntityPositionMessage (updates : Seq[EntityPositionUpdate])
case class EntityPositionUpdate (entityId : Long, position : ObjectCoord, velocity_inms : ReadVec3f)