package arx.tmp.game.networking.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/16/15
 * Time: 1:24 PM
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.Prelude._
import arx.core.datastructures.SynchronizedQueue
import arx.tmp.game.logic.command.TCommand
import arx.tmp.game.logic.world.data.TNetworkedWorldData
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import com.carrotsearch.hppc.IntOpenHashSet

class NetworkingWorldData extends TWorldAuxData with Externalizable {
	val auxDataInQueue = new IntOpenHashSet(1000)
	var isClient = false
	var clientId : Option[Int] = None

	protected var dirtyWorldData = Set[TNetworkedWorldData]()
	protected var dirtyAuxData = new SynchronizedQueue[TNetworkedGameEntityAuxData]()
	protected var commandsToSend = new SynchronizedQueue[TCommand]()

	var customCommunicators = Set[TCustomNetworkCommunicator]()
	
	def auxDataChanged(data: TNetworkedGameEntityAuxData) = {
		if (!isClient) {
			val hash = data.hashCode()
			if( ! auxDataInQueue.contains(hash) ) {
				auxDataInQueue.add(hash)
				dirtyAuxData.enqueue(data)
			}
		}
	}
	def nextChangedAuxData() = {
		if (!isClient) {
			val next = dirtyAuxData.dequeueOpt()
			for (n <- next) {
				auxDataInQueue.remove(n.hashCode())
			}
			next
		} else {
			None
		}
	}
	def hasChangedAuxData = if (!isClient) {
		!dirtyAuxData.isEmpty
	} else {
		false
	}


	def worldDataChanged (data : TNetworkedWorldData): Unit = {
		if (!isClient) {
			dirtyWorldData += data
		}
	}
	def fetchChangedWorldData = {
		val ret = dirtyWorldData
		dirtyWorldData = Set()
		ret
	}

	def addCommandToSend (command : TCommand): Unit = {
		commandsToSend.enqueue(command)
	}
	def hasCommandToSend : Boolean = {
		posit(isClient,"Checking command queue on server makes no sense")
		commandsToSend.nonEmpty
	}
	def nextCommandToSend = {
		commandsToSend.dequeueOpt()
	}
	def allCommandsToSend = {
		var commands = Vector[TCommand]()
		while (commandsToSend.nonEmpty) {
			commandsToSend.dequeueOpt() match {
				case Some(com) => commands :+= com
				case None => //...do nothing, obviously
			}
		}
		commands
	}



	override def readExternal(objectInput: ObjectInput): Unit = {
		dirtyAuxData = new SynchronizedQueue[TNetworkedGameEntityAuxData]()
	}
	override def writeExternal(objectOutput: ObjectOutput): Unit = {}
}

trait TCustomNetworkCommunicator {
	def messagesToSend () : Seq[AnyRef]
	def messageReceived (msg : Any) : Boolean
}

/** This is effectively an override of the networking world data that instead provides
  * a no-op, used on the client side where we do no direct back-syncing */
object ClientSideNetworkingWorldData extends NetworkingWorldData with NonDiscoverable {

}