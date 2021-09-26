/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


namespace RelLib/Lattice/Lifted {

    pub enum Lifted[a] with Eq, Order, ToString, Boxable { 
        case Bottom 
        case Lift(a)
    }

    instance Functor[Lifted] {
        pub def map(f: a -> b & ef, x: Lifted[a]): Lifted[b] & ef = match x {
            case Bottom => Bottom as & ef
            case Lift(a) => Lift(f(a))
        }
    }

    instance PartialOrder[Lifted[a]] with PartialOrder[a] {
        pub def lessEqual(x: Lifted[a], y: Lifted[a]): Bool = match (x, y) { 
            case (Bottom, _) => true
            case (_, Bottom) => false
            case (Lift(x1), Lift(y1)) => PartialOrder.lessEqual(x1, y1)
        }
    }

    instance LowerBound[Lifted[a]] {
        pub def minValue(): Lifted[a] = Bottom
    }

    instance UpperBound[Lifted[a]] with UpperBound[a] {
        pub def maxValue(): Lifted[a] = Lift(UpperBound.maxValue())
    }

    instance JoinLattice[Lifted[a]] with JoinLattice[a] {
        /// \/
        pub def leastUpperBound(x: Lifted[a], y: Lifted[a]): Lifted[a] = match (x, y) { 
            case (Bottom, y1) => y1
            case (x1, Bottom) => x1
            case (Lift(x1), Lift(y1)) => Lift(JoinLattice.leastUpperBound(x1, y1))
        }
    }

    instance MeetLattice[Lifted[a]] with MeetLattice[a] {
        /// /\
        pub def greatestLowerBound(x: Lifted[a], y: Lifted[a]): Lifted[a] = match (x, y) { 
            case (Bottom, _) => Bottom
            case (_, Bottom) => Bottom
            case (Lift(x1), Lift(y1)) => Lift(MeetLattice.greatestLowerBound(x1, y1))
        }
    }

}
