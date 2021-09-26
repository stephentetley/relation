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

namespace RelLib/Lattice/Min {

    use RelLib/Lattice/Dropped.{Dropped};
    use RelLib/Lattice/Dropped.Dropped.{Top, Drop};

    pub enum Min[a] with Eq, Order, ToString, Boxable { case Min(Dropped[a]) }

    pub def min1(x: a): Min[a] = Min(Drop(x))

    instance Functor[Min] {
        pub def map(f: a -> b & ef, x: Min[a]): Min[b] & ef = match x {
            case Min(a) => Min(Functor.map(f, a))
        }
    }

    /// Seems a bit disappointing to need Oder for Partial order, can this be improved?
    instance PartialOrder[Min[a]] with Order[a] {
        pub def lessEqual(x: Min[a], y: Min[a]): Bool = match (x, y) { 
            case (Min(x1), Min(y1)) => RelLib/Lattice/Dropped.lessEqualWith((a,b) -> a <= b, x1, y1)
        }
    }

    instance LowerBound[Min[a]] with LowerBound[a] {
        pub def minValue(): Min[a] = Min(LowerBound.minValue())
    }


    instance UpperBound[Min[a]] {
        pub def maxValue(): Min[a] = Min(UpperBound.maxValue())
    }

    instance JoinLattice[Min[a]] with Order[a] {
        /// \/
        pub def leastUpperBound(x: Min[a], y: Min[a]): Min[a] = 
            let lub = (a, b) -> Drop(Order.min(a, b));
            match (x, y) {
                case (Min(x1), Min(y1)) => Min(RelLib/Lattice/Dropped.leastUpperBoundWith(lub, x1, y1))
            }
    }

    instance MeetLattice[Min[a]] with Order[a] {
        /// /\
        pub def greatestLowerBound(x: Min[a], y: Min[a]): Min[a] = 
            let glb = (a, b) -> Drop(Order.max(a,b));
            match (x, y) {
                case (Min(x1), Min(y1)) => Min(RelLib/Lattice/Dropped.greatestLowerBoundWith(glb, x1, y1))
            }
    }

}
