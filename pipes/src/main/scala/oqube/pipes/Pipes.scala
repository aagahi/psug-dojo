/**
 *  Copyright (C) 2008 - OQube / Arnaud Bailly
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Created on Tue Feb 19 2008
 */
package oqube.pipes

trait Observable 

/**
 * 
 * @author abailly@oqube.com
 * @version $Rev$
 */
case class Observation(time: Int, value: Double) extends Observable {

  private[this] var _next : Option[Observation] = None

  def next_=(obs : Observation) = 
    if(obs.time <= this.time) 
      throw new IllegalArgumentException("cannot set next to an observation at an earlier date") 
    else 
      _next = Some(obs)

  def next : Option[Observation] = _next

  private[this] var _prev : Option[Observation] = None

  def previous_=(obs : Observation) = 
    if(obs.time >= this.time) 
      throw new IllegalArgumentException("cannot set previous to an observation at a later date") 
    else 
      _prev = Some(obs)

  def previous : Option[Observation] = _prev

  def interpolate(from: Observation, to: Observation, at: Int) : Interpolated  = 
    Interpolated(at
		 ,from.value 
		 + ((to.value - from.value) * (at - from.time) 
		    / (to.time - from.time)
		  ))
  
  def observable(at : Int) : Observable = {
    if(at < time) 
      interpolate(_prev.getOrElse(Observation(at,value)),this,at)
    else if(at > time) 
      interpolate(this,_next.getOrElse(Observation(at,value)),at)
    else
      this
  }
}

case class Experiment(start: Int, end: Int) {

  var observations : List[Observation] = Nil

  var first : Observation = _
  
  def ++(obs : Observation) : Experiment = {
    observations = observations match {
      case Nil     => first = obs; obs :: Nil
      case o :: os => o.next = obs; obs.previous = o; o :: observations
    }
    this
  }
  
  def apply(time : Int) : Observable = first.observable(time)
}

case class Interpolated(time: Int, value: Double) extends Observable 
