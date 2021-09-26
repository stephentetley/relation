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


namespace RelLib/Lattice/Dropped {

    pub enum Dropped[a] with Eq, Order, ToString, Boxable { 
        case Drop(a)
        case Top
    }

    instance Functor[Dropped] {
        pub def map(f: a -> b & ef, x: Dropped[a]): Dropped[b] & ef = match x {
            case Drop(a) => Drop(f(a))
            case Top => Top as & ef
        }
    }

    instance PartialOrder[Dropped[a]] with PartialOrder[a] {
        pub def lessEqual(x: Dropped[a], y: Dropped[a]): Bool = match (x, y) { 
            case (_, Top) => true
            case (Top, _) => false
            case (Drop(x1), Drop(y1)) => PartialOrder.lessEqual(x1, y1)
        }
    }

    pub def lessEqualWith(le: (a, a) -> Bool, x: Dropped[a], y: Dropped[a]): Bool = match (x, y) { 
        case (_, Top) => true
        case (Top, _) => false
        case (Drop(x1), Drop(y1)) => le(x1, y1)
    }

    instance LowerBound[Dropped[a]] with LowerBound[a] {
        pub def minValue(): Dropped[a] = Drop(LowerBound.minValue())
    }

    pub def minValueWith(x: a): Dropped[a] = Drop(x)

    instance UpperBound[Dropped[a]] {
        pub def maxValue(): Dropped[a] = Top
    }

    instance JoinLattice[Dropped[a]] with JoinLattice[a] {
        /// \/
        pub def leastUpperBound(x: Dropped[a], y: Dropped[a]): Dropped[a] = match (x, y) { 
            case (Top, _) => Top
            case (_, Top) => Top
            case (Drop(x1), Drop(y1)) => Drop(JoinLattice.leastUpperBound(x1, y1))
        } 
    }

    pub def leastUpperBoundWith(lub: (a, a) -> a, x: Dropped[a], y: Dropped[a]): Dropped[a] = match (x, y) { 
        case (Top, _) => Top
        case (_, Top) => Top
        case (Drop(x1), Drop(y1)) => Drop(lub(x1, y1))
    } 

    instance MeetLattice[Dropped[a]] with MeetLattice[a] {
        /// /\
        pub def greatestLowerBound(x: Dropped[a], y: Dropped[a]): Dropped[a] = match (x, y) { 
            case (Top, y1) => y1
            case (x1, Top) => x1
            case (Drop(x1), Drop(y1)) => Drop(MeetLattice.greatestLowerBound(x1, y1))
        }
    }

    pub def greatestLowerBoundWith(glb: (a, a) -> a, x: Dropped[a], y: Dropped[a]): Dropped[a] = match (x, y) { 
        case (Top, y1) => y1
        case (x1, Top) => x1
        case (Drop(x1), Drop(y1)) => Drop(glb(x1, y1))
    }
}
