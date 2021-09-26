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


namespace RelLib/Lattice/Levitated {

    pub enum Levitated[a] with Eq, Order, ToString, Boxable { 
        case Bottom
        case Levitate(a)
        case Top
    }

    instance Functor[Levitated] {
        pub def map(f: a -> b & ef, x: Levitated[a]): Levitated[b] & ef = match x {
            case Bottom => Bottom as & ef
            case Levitate(a) => Levitate(f(a))
            case Top => Top as & ef
        }
    }

    instance PartialOrder[Levitated[a]] with PartialOrder[a] {
        pub def lessEqual(x: Levitated[a], y: Levitated[a]): Bool = match (x, y) { 
            case (_, Top) => true
            case (Top, _) => false
            case (Bottom, _) => true
            case (_, Bottom) => false            
            case (Levitate(x1), Levitate(y1)) => PartialOrder.lessEqual(x1, y1)
        }
    }

    pub def lessEqualWith(le: (a, a) -> Bool, x: Levitated[a], y: Levitated[a]): Bool = match (x, y) { 
            case (_, Top) => true
            case (Top, _) => false
            case (Bottom, _) => true
            case (_, Bottom) => false            
            case (Levitate(x1), Levitate(y1)) => le(x1, y1)
        }

    instance LowerBound[Levitated[a]] {
        pub def minValue(): Levitated[a] = Bottom
    }

    instance UpperBound[Levitated[a]] {
        pub def maxValue(): Levitated[a] = Top
    }

    instance JoinLattice[Levitated[a]] with JoinLattice[a] {
        /// \/
        pub def leastUpperBound(x: Levitated[a], y: Levitated[a]): Levitated[a] = match (x, y) { 
            case (Top, _) => Top
            case (_, Top) => Top
            case (Levitate(x1), Levitate(y1)) => Levitate(JoinLattice.leastUpperBound(x1, y1))
            case (Bottom, y1) => y1
            case (x1, Bottom) => x1
        }
    }

    pub def leastUpperBoundWith(lub: (a, a) -> Levitated[a], x: Levitated[a], y: Levitated[a]): Levitated[a] = match (x, y) { 
            case (Top, _) => Top
            case (_, Top) => Top
            case (Levitate(x1), Levitate(y1)) => lub(x1, y1)
            case (Bottom, y1) => y1
            case (x1, Bottom) => x1
        }

    instance MeetLattice[Levitated[a]] with MeetLattice[a] {
        /// /\
        pub def greatestLowerBound(x: Levitated[a], y: Levitated[a]): Levitated[a] = match (x, y) { 
            case (Top, y1) => y1
            case (x1, Top) => x1
            case (Levitate(x1), Levitate(y1)) => Levitate(MeetLattice.greatestLowerBound(x1, y1))
            case (Bottom, _) => Bottom
            case (_, Bottom) => Bottom
        }
    }

    pub def greatestLowerBoundWith(gub: (a, a) -> Levitated[a], x: Levitated[a], y: Levitated[a]): Levitated[a] = match (x, y) { 
            case (Top, y1) => y1
            case (x1, Top) => x1
            case (Levitate(x1), Levitate(y1)) => gub(x1, y1)
            case (Bottom, _) => Bottom
            case (_, Bottom) => Bottom
        }

}
