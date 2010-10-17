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
 * Created on Thu Jan 24 2008
 */
package oqube.lags

object naive {

  import request._

  def sublists(list : List[Request]) : List[List[Request]] = list match {
    case Nil      => List(Nil)
    case x :: xs  => {
      val sub = sublists(xs)
      sub.map(x :: _) ::: sub
    }
  }

  def isAcceptable(list : List[Request]) : Boolean = list match {
    case Nil     => true
    case x :: xs => xs.foldLeft(true)((a,r) => a && (x >>> r)) && isAcceptable(xs)
  }

  def max(a :Int, b: Int) : Int = if(a > b) a else b

  def bestTurnover(list : List[Request]) : Int = 
    sublists(list)
      .filter(isAcceptable _)
      .map(turnover)
      .foldLeft(0)(max)
  
}

object request {

  import naive.max

  /**
   * Implicit conversion for easy comparison of requests.
   */
  implicit def requestAsInt(r : Request) : Double = r.start
  
  def turnover(list : List[Request]) : Int = list.foldLeft(0)(_ + _.price)

  /**
   * 
   * @author abailly@oqube.com
   * @version $Rev$
   */
   case class Request(start : Int, end : Int, price :Int) {
     def >>>(r : Request) = r.start >= end
   }

  implicit def extractSchedule(maybe : List[Schedule]) : Schedule = maybe match {
    case x :: y :: xs => y
    case x :: xs => x
  }

  implicit def schedToIterable(s : Schedule) : List[Request] = s.schedule

  def makeSchedules( l : List[Request]) : List[Schedule] = {
    l.foldLeft(List(new Schedule))((scheds,r) => scheds.flatMap(s => s <+> r))
  }
   
  def maximalTurnover(l : List[Request]) : Int = makeSchedules(l).map(_.turnover).foldLeft(0)(max)

  class Schedule {
    
    // stored in reverse insertion order
    var reqs : List[Request] = Nil

    def schedule : List[Request] = this.reqs.reverse

    def turnover = reqs.foldLeft(0)(_ + _.price)

    def this(ls : List[Request]) = {
      this()
      this.reqs = ls
    }

    def <+>(req : Request) : List[Schedule] = {
      reqs match {
	  case x :: xs  if(! (x >>> req))  => List(this, new Schedule(req :: xs))
	  case _                           => this.reqs = req :: reqs; List(this)
	}
    }

    override def equals(that : Any) : boolean = this.reqs == that.asInstanceOf[Schedule].reqs

    override def toString(): String = reqs.toString
  }      
}
