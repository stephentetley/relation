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

/// Turn an ordered type into a lattice...

namespace RelLib/Lattice/Trans/Ordered {

    pub enum Ordered[a] with Eq, Order, ToString, Boxable { case Ordered(a) }


    instance Functor[Ordered] {
        pub def map(f: a -> b & ef, x: Ordered[a]): Ordered[b] & ef = match x {
            case Ordered(a) => Ordered(f(a))
        }
    }


    instance PartialOrder[Ordered[a]] with Order[a] {
        pub def lessEqual(x: Ordered[a], y: Ordered[a]): Bool = match (x, y) {
            case (Ordered(x1), Ordered(y1)) => x1 <= y1
        }
    }

    /// Note - this is due to be changed if Flix gets a Bounded type class
    instance LowerBound[Ordered[a]] with LowerBound[a] {
        pub def minValue(): Ordered[a] = Ordered(LowerBound.minValue())
    }

    /// Note - this is due to be changed if Flix gets a Bounded type class
    instance UpperBound[Ordered[a]] with UpperBound[a] {
        pub def maxValue(): Ordered[a] = Ordered(UpperBound.maxValue())
    }


    instance JoinLattice[Ordered[a]] with Order[a] {
        /// \/
        pub def leastUpperBound(x: Ordered[a], y: Ordered[a]): Ordered[a] = match (x, y) {
            case (Ordered(x1), Ordered(y1)) => Ordered(Order.max(x1, y1))
        }
    }

    instance MeetLattice[Ordered[a]] with Order[a] {
        /// /\
        pub def greatestLowerBound(x: Ordered[a], y: Ordered[a]): Ordered[a] = match (x, y) {
            case (Ordered(x1), Ordered(y1)) => Ordered(Order.min(x1, y1))
        }
    }

}
