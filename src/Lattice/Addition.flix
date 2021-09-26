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

/// WARNING - not sure of the semantics of this one...

namespace RelLib/Lattice/Addition {

    pub enum Addition[a] with Eq, Order, ToString, Boxable { 
        case Top
        case Addition(a) 
        case Bottom    
    }

    instance PartialOrder[Addition[a]] with Eq[a] {
        pub def lessEqual(x: Addition[a], y: Addition[a]): Bool = match (x, y) { 
            case (Bottom, _) => true
            case (Top, Bottom) => false
            case (Top, Addition(_)) => false
            case (Top, Top) => true
            case (Addition(_), Bottom) => false
            case (Addition(_), Top) => true
            case (Addition(x1), Addition(y1)) => x1 == y1
        }
    }

    instance LowerBound[Addition[a]] {
        pub def minValue(): Addition[a] = Bottom
    }

    instance UpperBound[Addition[a]] {
        pub def maxValue(): Addition[a] = Top
    }

    instance JoinLattice[Addition[a]] with Add[a] {
        /// \/
        pub def leastUpperBound(x: Addition[a], y: Addition[a]): Addition[a] = match (x, y) { 
            case (Top, _) => Top
            case (Bottom, y1) => y1
            case (Addition(_), Top) => Top
            case (Addition(x1), Bottom) => Addition(x1)
            case (Addition(x1), Addition(y1)) => Addition(x1 + y1)
        }
    }

    instance MeetLattice[Addition[a]] with Add[a]{
        /// /\
        pub def greatestLowerBound(x: Addition[a], y: Addition[a]): Addition[a] = match (x, y) { 
            case (Bottom, _) => Bottom
            case (Top, y1) => y1
            case (Addition(_), Bottom) => Bottom
            case (Addition(x1), Top) => Addition(x1)    
            case (Addition(x1), Addition(y1)) => Addition(x1 + y1)
        }
    }

}