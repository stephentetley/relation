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

    instance PartialOrder[Min[a]] with Eq[a] {
        pub def lessEqual(x: Min[a], y: Min[a]): Bool = match (x, y) { 
            case (Min(x1), Min(y1)) => RelLib/Lattice/Dropped.lessEqualWith((a,b) -> a == b, x1, y1)
        }
    }

    instance LowerBound[Min[a]] with LowerBound[a] {
        pub def minValue(): Min[a] = Min(RelLib/Lattice/Dropped.minValueWith(LowerBound.minValue()))
    }


    instance UpperBound[Min[a]] {
        pub def maxValue(): Min[a] = Min(UpperBound.maxValue())
    }

    def maxpo(x: a, y: a): a with PartialOrder[a] = 
        if (PartialOrder.lessEqual(x, y)) y else x

    def minpo(x: a, y: a): a with PartialOrder[a] = 
        if (PartialOrder.lessEqual(x, y)) x else y
    


    instance JoinLattice[Min[a]] with PartialOrder[a] {
        /// \/
        pub def leastUpperBound(x: Min[a], y: Min[a]): Min[a] =match (x, y) {
            case (Min(x1), Min(y1)) => Min(RelLib/Lattice/Dropped.leastUpperBoundWith(minpo, x1, y1))
        }
    }

    instance MeetLattice[Min[a]] with PartialOrder[a] {
        /// /\
        pub def greatestLowerBound(x: Min[a], y: Min[a]): Min[a] = match (x, y) {
            case (Min(x1), Min(y1)) => Min(RelLib/Lattice/Dropped.greatestLowerBoundWith(maxpo, x1, y1))
        }
    }

}
