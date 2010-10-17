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

/*
 * needed imports for specs
 */
import org.specs.runner.JUnit4
import org.specs._
import request._
import naive._

/* use scalacheck for high-level properties */
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalacheck.Gen._


/**
 * Specification of LAGS problem.
 */
object LagsSpecification extends Specification {
  
  "a reservation request" should {

    "be defined by its start, end hour and price" in {
      val d = Request(1,4,50)
      d.start must be(1)
      d.end must be(4)
      d.price must be(50)
    }

    "be comparable by start date to some other request" in {
      val d = Request(1, 4 ,50)
      val d2 = Request(3, 4 ,50)
      d2 must beGreaterThan(d)
    }

    "be compatible with other request if times do not overlap" in {
      val d = Request(1, 4 ,50)
      val d3 = Request(4, 5 ,50)
      d >>> d3 must be(true)
    }

    "be incompatible with some other request if times do overlap" in {
      val d = Request(1, 4 ,50)
      val d2 = Request(3, 4 ,50)
      d >>> d2 must be(false)
    }

    "be incompatible with some other request if it includes it" in {
      val d = Request(1, 4 ,50)
      val d2 = Request(3, 2 ,50)
      d >>> d2 must be(false)
    }

    "be incompatible with some other request if it is included in it" in {
      val d = Request(3, 4 ,50)
      val d2 = Request(1, 12,50)
      d >>> d2 must be(false)
    }

    "be incompatible with some other request if it is overlapped" in {
      val d = Request(3, 5 ,50)
      val d2 = Request(1, 4,50)
      d >>> d2 must be(false)
    }
  }

  "the sublists of a list" should {
    
    val norequest : List[Request] = Nil

    "be the singleton nil for empty list" in {
      val l = Nil
      sublists(l) must be_==(List(norequest))
    }

    "be a 2-element list for a singleton list" in {
      val l = List(Request(1,2,3))
      val subs = sublists(l)
      subs.length must be(2)
      subs must contain(List(Request(1,2,3)))
      subs must contain(norequest)
    }

    "be a 4-element list for a 2-element list" in {
      val l = List(Request(1,2,3),Request(2,3,4))
      val subs = sublists(l)
      subs.length must be(4)
      subs must contain(List(Request(1,2,3),Request(2,3,4)))
    }

    "be a 16-element list for a 4-element list" in {
      val l = List(Request(1,2,3),Request(2,3,4),Request(3,3,4),Request(4,5,4))
      val subs = sublists(l)
      subs.length must be(16)
      subs must contain(List(Request(1,2,3),Request(4,5,4)))
    }
  }

  "checking compatible lists" should {
    
    "an empty list is acceptable" in {
      val list = Nil
      isAcceptable(list) must be(true)
    }

    "a list with 2 incompatible elements is not acceptable" in {
      val list = List(Request(1,3,3),Request(2,4,3))
      isAcceptable(list) must be(false)
    }

    "a list with 1 element is compatible" in {
      val list = List(Request(1,3,3))
      isAcceptable(list) must be(true)
    }

    "a list with 2 incompatible elements not at first place is not acceptable" in {
      val list = List(Request(1,3,3),Request(3,5,3),Request(4,5,3))
      isAcceptable(list) must be(false)
    }
    
  }

  "a maximal turnover from a list" should {
      
    "be 0 for an empty list" in {
      val list = Nil
      bestTurnover(list) must be(0)
    }

    "be its turnover for a singleton list" in {
      val req = Request(1,3,3)
      val list = List(req)
      bestTurnover(list) must be(req.price)
    }

    "be the sum of price for an acceptable list with more than 1 element" in {
      val list = List(Request(1,3,3),Request(3,5,6))
      bestTurnover(list) must be(9)
    }

    
  }
}

object LagsObjectSpecification extends Specification {

  "a more efficient scheduling algorithm".isSpecifiedBy(LagsSpecification)

  val requests = for {
    d <- Gen.choose(0,23)
    f <- Gen.choose(0,23) suchThat ( _ > d)
    p <- Gen.choose(1,100)
  } yield new Request(d,f,p)
  
  implicit def sortedRequest (g : => Gen[Request]) : Gen[List[Request]] = Gen.sized(size => for {
    n <- choose(0,size)
    l <- vectorOf(n,g)
  } yield l.toList.sort(_ < _))
  

  "a lags schedule" should {

    "be constructed empty" in {
      (new Schedule).schedule must be_==(Nil)
    }

    "allow initial addition of a request and store it" in {
      val sched = new Schedule
      val req = Request(1,1,2)
      (sched <+> req).head.schedule must be_==(List(req))
      
    }

    "store two compatible requests added in order" in {
      val sched = new Schedule
      val r1 = Request(1,2,2)
      val r2 = Request(3,4,2)
      (sched <+> r1 <+> r2).head.schedule must be_==(List(r1,r2))
    }

    "return new schedule if an incompatible request is added to one element list" in {
      val sched = new Schedule
      val r1 = Request(1,3,2)
      val r2 = Request(2,4,2)
      val s = sched <+> r1 <+> r2 
      s.head.schedule must be_==(List(r1))
      s.tail.head.schedule must be_==(List(r2))
    }
    
    "store three compatible requests added in order" in {
      val sched = new Schedule
      val r1 = Request(1,2,2)
      val r2 = Request(3,4,2)
      val r3 = Request(5,6,2)
      (sched <+> r1 <+> r2 <+> r3).schedule must be_==(List(r1,r2,r3))
    }

    "return new schedule if an incompatible request is added to 2-element list" in {
      val sched = new Schedule
      val r1 = Request(1,3,2)
      val r2 = Request(3,5,2)
      val r3 = Request(4,6,2)
      val s : Schedule = sched <+> r1 <+> r2 <+> r3
      sched.schedule must be_==(List(r1,r2))
      s.schedule must be_==(List(r1,r3))
    }

  }

  "a list of ordered requests" should {
    
    "produce an empty schedule if empty" in {
      val list : List[Request] = Nil
      makeSchedules(list) must be_==(List(new Schedule))
    }

    "produce a single schedule if a singleton" in {
      val list : List[Request] = List(Request(1,2,3))
      val scheds = makeSchedules(list)
      scheds.length must be(1)
      scheds(0).schedule must be_==(List(Request(1,2,3)))
    }

    "produce 2 schedules if contains 2 incompatible requests" in {
      val list : List[Request] = List(Request(1,3,3),Request(2,4,3))
      val scheds : List[Schedule] = makeSchedules(list)
      scheds.length must be(2)
    }

    "produce 1 schedules if contains 2 compatible requests" in {
      val list : List[Request] = List(Request(1,3,3),Request(5,7,3))
      val scheds : List[Schedule] = makeSchedules(list)
      scheds.length must be(1)
    }


    "produce 1 schedules if contains 4 compatible requests" in {
      val list : List[Request] = List(Request(1,3,3),Request(5,7,3),Request(8,12,3),Request(15,20,3))
      val scheds : List[Schedule] = makeSchedules(list)
      scheds.length must be(1)
    }

    "allow computation of maximal turnover" in {
      
      val prop = forAll( sortedRequest(requests) ) {
      	ds : List[Request] =>
  	      maximalTurnover(ds) == bestTurnover(ds)
        }
      prop must pass( display(minTestsOk -> 50, maxDiscarded -> 2000) )
    }
    
  }
}
  


/**
 * Tests pour le projet LAGS.
 * @author abailly@oqube.com
 * @version $Rev$
 */
class LagsTest extends JUnit4(LagsObjectSpecification) 
