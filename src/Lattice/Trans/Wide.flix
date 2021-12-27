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

/// Graft Top and Bottom onto any type that supports Eq...

namespace RelLib/Lattice/Trans/Wide {

    pub enum Wide[a] with Eq, Order, ToString, Boxable { 
        case Bottom
        case Middle(a)
        case Top
    }

    instance Functor[Wide] {
        pub def map(f: a -> b & ef, x: Wide[a]): Wide[b] & ef = match x {
            case Bottom => Bottom as & ef
            case Middle(a) => Middle(f(a))
            case Top => Top as & ef
        }
    }

    instance PartialOrder[Wide[a]] with Eq[a] {
        pub def lessEqual(x: Wide[a], y: Wide[a]): Bool = match (x, y) { 
            case (Bottom, _) => true
            case (Top, Bottom) => false
            case (Top, Middle(_)) => false
            case (Top, Top) => true
            case (Middle(_), Bottom) => false
            case (Middle(_), Top) => true
            case (Middle(x1), Middle(y1)) => x1 == y1
        }
    }

    instance LowerBound[Wide[a]] {
        pub def minValue(): Wide[a] = Bottom
    }

    instance UpperBound[Wide[a]] {
        pub def maxValue(): Wide[a] = Top
    }

    instance JoinLattice[Wide[a]] with Eq[a] {
        /// \/
        pub def leastUpperBound(x: Wide[a], y: Wide[a]): Wide[a] = match (x, y) { 
            case (Top, _) => Top
            case (Bottom, y1) => y1
            case (Middle(_), Top) => Top
            case (Middle(x1), Bottom) => Middle(x1)
            case (Middle(x1), Middle(y1)) => if (x1 == y1) Middle(x1) else Top
        }
    }

    instance MeetLattice[Wide[a]] with Eq[a] {
        /// /\
        pub def greatestLowerBound(x: Wide[a], y: Wide[a]): Wide[a] = match (x, y) { 
            case (Bottom, _) => Bottom
            case (Top, y1) => y1
            case (Middle(_), Bottom) => Bottom
            case (Middle(x1), Top) => Middle(x1)    
            case (Middle(x1), Middle(y1)) => if (x1 == y1) Middle(x1) else Bottom
        }
    }



}
