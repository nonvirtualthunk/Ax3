package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/12/12
 * Time: 9:40 AM
 * Created by nonvirtualthunk
 */

trait TLoggingTalea[T] extends ITalea[T] {
	var _loggedModifications = List[ LoggedTaleaModification[T] ]()
	def loggedModifications : List[ LoggedTaleaModification[T] ] = _loggedModifications
	def loggedModifications_= ( l : List[ LoggedTaleaModification[T] ] ) { _loggedModifications = l }
}