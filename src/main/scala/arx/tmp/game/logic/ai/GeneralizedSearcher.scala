package arx.tmp.game.logic.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/9/13
 * Time: 7:41 PM
 */

import arx.core.FibonacciHeap
import arx.core.THasSortKey

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GeneralizedSearcher {

	case class SearchQuery[T] (
										 from : T,
										 maxCost : Int,
										 heuristicFunction : (T,Float) => Float,
										 searchLimitFunction : (T) => Boolean,
										 endFunction : (T) => Boolean,
										 successorFunction : (T,mutable.Buffer[T]) => Unit,
										 costFunction : (T,T) => Float
									)

	case class SearchNode[T] ( v : T , var g : Float , h : Float, var parent : SearchNode[T] ) extends THasSortKey {
		def sortKey: Float = h + g * 0.001f
		def path : List[T] = parent match {
			case null => List(v)
			case p => p.path :+ v
		}
	}

	def search[T] ( params : SearchQuery[T] ) : mutable.HashMap[T,SearchNode[T]] = {
		import params._

		val heap = new FibonacciHeap[SearchNode[T]]
		heap.enqueue( SearchNode[T](from,0,heuristicFunction(from,0),null) )

		val closedSet = new mutable.HashSet[T]
		val resultMap = new mutable.HashMap[T,SearchNode[T]]
		val openMap = new mutable.HashMap[T,FibonacciHeap.Node[SearchNode[T]]]

		val successors = new ListBuffer[T]


		var nodecount = 0
		while ( heap.nonEmpty && heap.peek.g <= maxCost ) {
			val node = heap.dequeue()
			if ( ! closedSet.contains( node.v ) ) {

				if ( endFunction(node.v) ) { return resultMap }
				//test if this is a valid space to occupy
				closedSet.add(node.v)

				resultMap(node.v) = node

				successors.clear()
				successorFunction(node.v,successors)

				for ( successor <- successors ) {
					if ( ! closedSet.contains( successor ) && searchLimitFunction(successor) ) {
						val newG = node.g + costFunction(node.v,successor)
						openMap.get(successor) match {
							case Some(existing) => {
								if ( existing.data.g < newG ) { }
								else {
									nodecount += 1
									existing.data.g = newG
									existing.data.parent = node
									heap.updateKey( existing )
								}
							}
							case _ => {
								nodecount += 1
								val newNode = SearchNode[T]( successor , newG , heuristicFunction(successor,newG), node )
								val fn = heap.enqueue( newNode )
								openMap( newNode.v ) = fn
							}
						}
					}
				}
			}
		}

		println(s"nodecount : $nodecount")

		resultMap
	}

}
