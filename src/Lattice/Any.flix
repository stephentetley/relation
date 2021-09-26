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

namespace RelLib/Lattice/Any {



    pub enum Any with Eq, Order, ToString, Boxable { 
        case Bottom
        case Any(Bool)
        case Top
    }

    instance PartialOrder[Any] {
        pub def lessEqual(x: Any, y: Any): Bool = match (x, y) { 
            case (Bottom, _) => true
            case (Top, Bottom) => false
            case (Top, Any(_)) => false
            case (Top, Top) => true
            case (Any(_), Bottom) => false
            case (Any(_), Top) => true
            case (Any(x1), Any(y1)) => x1 == y1
        }
    }

    instance LowerBound[Any] {
        pub def minValue(): Any = Bottom
    }

    instance UpperBound[Any] {
        pub def maxValue(): Any = Top
    }

    instance JoinLattice[Any] {
        /// \/
        pub def leastUpperBound(x: Any, y: Any): Any = match (x, y) { 
            case (Top, _) => Top
            case (Bottom, y1) => y1
            case (Any(_), Top) => Top
            case (Any(x1), Bottom) => Any(x1)
            case (Any(x1), Any(y1)) => if (x1 or y1) Any(true) else Top
        }
    }

    instance MeetLattice[Any] {
        /// /\
        pub def greatestLowerBound(x: Any, y: Any): Any = match (x, y) { 
            case (Bottom, _) => Bottom
            case (Top, y1) => y1
            case (Any(_), Bottom) => Bottom
            case (Any(x1), Top) => Any(x1)    
            case (Any(x1), Any(y1)) => if (x1 or y1) Any(true) else Bottom
        }
    }

}
