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



namespace RelLib/Lattice/PowerSet {

    pub enum PowerSet[a](Set[a]) with ToString, Boxable

    instance Eq[PowerSet[a]] with Order[a] {
        pub def eq(x: PowerSet[a], y: PowerSet[a]): Bool = match (x, y) {
            case (PowerSet(x1), PowerSet(y1)) => Eq.eq(x1, y1)
        }
    }

    instance Order[PowerSet[a]] with Order[a] {
        pub def compare(x: PowerSet[a], y: PowerSet[a]): Comparison = match (x, y) {
            case (PowerSet(x1), PowerSet(y1)) => Order.compare(x1, y1)
        }
    }

    pub def powerset(x: Set[a]): PowerSet[a] = PowerSet(x)

    

    instance PartialOrder[PowerSet[a]] with Order[a] {
        pub def lessEqual(x: PowerSet[a], y: PowerSet[a]): Bool = match (x, y) { 
            case (PowerSet(x1), PowerSet(y1)) => x1 <= y1
        }
    }

    instance LowerBound[PowerSet[a]] {
        pub def minValue(): PowerSet[a] = PowerSet(Set.empty())
    }

    /// No UpperBound unless we have Top constructor (is this true?)

    instance JoinLattice[PowerSet[a]] with Order[a] {
        /// \/
        pub def leastUpperBound(x: PowerSet[a], y: PowerSet[a]): PowerSet[a] = match (x, y) { 
            case (PowerSet(x1), PowerSet(y1)) => PowerSet(Set.union(x1, y1))
        }
    }

    instance MeetLattice[PowerSet[a]] with Order[a] {
        /// /\
        pub def greatestLowerBound(x: PowerSet[a], y: PowerSet[a]): PowerSet[a] = match (x, y) { 
            case (PowerSet(x1), PowerSet(y1)) => PowerSet(Set.intersection(x1, y1))
        }
    }



}
