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

/*
 * needed imports for specs
 */
import junit.framework._
import org.specs.runner.JUnit3
import org.specs._

/**
 * 
 * @author abailly@oqube.com
 * @version $Rev$
 */
object PipesSpecification extends Specification {

  "an observation" should {
    "be defined by a timestamp and a value" in {
      val obs = Observation(1,1.0)
      obs.time must be(1)
      obs.value must beCloseTo(1.0, 0.0001)
    }

    "provide itself as observable for its timestamp" in {
      val obs = Observation(1,1.0)
      obs.observable(1) must be(obs)
    }

    "provide interpolated on requested time if asked for previous observable" in {
      val obs = Observation(1,1.0)
      obs.observable(0) must be_==(Interpolated(0,1.0))
    }

    "provide interpolated on requested time if asked for further observable" in {
      val obs = Observation(1,1.0)
      obs.observable(3) must be_==(Interpolated(3,1.0))
    }

    "provide interpolated value with next observation if asked for time in between" in {
      val obs = Observation(1,1.0)
      obs.next = Observation(5,5.0)
      obs.observable(3) must be_==(Interpolated(3,3.0))
    }
    
    "provide interpolated value with previous observation if asked for time in between" in {
      val obs = Observation(5,5.0)
      obs.previous = Observation(1,1.0)
      obs.observable(2) must be_==(Interpolated(2,2.0))
    }
   
    "not allow setting previous observation to a value further in time" in {
      val obs = Observation(2,5.0)
      (obs.previous = Observation(5,1.0)) must throwA(new Exception)
    }

    "not allow setting next observation to a value before in time" in {
      val obs = Observation(5,5.0)
      (obs.next = Observation(1,1.0)) must throwA(new Exception)
    }
  }

  "an experiment" should {
    
    "be defined by a range" in {
      val exp = Experiment(0,10)
      exp.start must be(0)
      exp.end   must be(10)
    }

    "allow addition and retrieval of observations" in {
      var exp = Experiment(0,10)
      exp = exp ++ Observation(2,5.0)
      exp(2) must be_==(Observation(2,5.0))
    }

    "allow addition and retrieval of any observations" in {
      var exp = Experiment(0,10)
      exp = exp ++ Observation(2,5.0) ++ Observation(5,3.0) ++ Observation(11,7.0)
      exp(5) must be_==(Observation(5,3.0))
    }

    "allow  retrieval of interpolated values" in {
      var exp = Experiment(0,10)
      exp = exp ++ Observation(2,5.0) ++ Observation(5,2.0)
      exp(4) must be_==(Interpolated(4,3.0))
    }
  }
}

class PipesTest extends JUnit3(PipesSpecification) 

