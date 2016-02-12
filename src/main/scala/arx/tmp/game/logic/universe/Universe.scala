package arx.tmp.game.logic.universe

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/14/13
 * Time: 4:20 PM
 * Created by nonvirtualthunk
 */

import java.io.File

import arx.Prelude._
import arx.application.Application
import arx.core.async.Async
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.tmp.game.logic.entities.core.THasAuxData
import arx.resource.ResourceManager

@SerialVersionUID(1L)
class Universe extends Serializable with THasAuxData[TUniverseData] with TSentinelable {

}

object Universe {
	protected val _universe = Async.submit(() => {
		val universeFile = new File("./save/universe.dat")
		val generateAnew = false
		if ( universeFile.exists && ! generateAnew ) {
			val universe = readFromFile[Universe](universeFile)
			universe
		} else {
			val gen = new UniverseGenerator
			val universe = gen.generateUniverse()
			val tmpFile = File.createTempFile("universe","tmp")
			writeToFile(universeFile,universe)
//			tmpFile.renameTo(universeFile)
			universe
		}
	})

	def universe = _universe.get()

	var uidCounter = {
		val f = ResourceManager.getSaveFile("uid.dat")
		if (! f.exists){
			1l
		} else {
			readFromFile[Long](f)
		}
	}
	Application.onQuit(() => {
		val f = ResourceManager.getSaveFile("uid.dat")
		writeToFile(f,uidCounter)
	})
}

object SentinelUniverse extends Universe with TSentinel {

}