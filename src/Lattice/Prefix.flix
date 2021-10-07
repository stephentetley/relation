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

namespace RelLib/Lattice/Prefix {



    pub enum Prefix with Eq, Order, ToString, Boxable {
        case Bottom 
        case Pre(String)
    }

    /// pub def min1(x: a): Prefix = Min(Ordered(x))

    pub def getValue(x: Prefix): Option[String] = match x {
        case Bottom => None
        case Pre(s) => Some(s)
    }

    /// This might be the problem...
    instance PartialOrder[Prefix] {
        pub def lessEqual(x: Prefix, y: Prefix): Bool = match (x, y) { 
            case (Bottom, _) => true
            case (_, Bottom) => false
            case (Pre(x1), Pre(y1)) => x1 `String.startsWith` y1
        }
    }

    instance LowerBound[Prefix] {
        pub def minValue(): Prefix = Bottom
    }


    instance UpperBound[Prefix] {
        pub def maxValue(): Prefix = Pre("")
    }


    instance JoinLattice[Prefix] {
        /// \/
        pub def leastUpperBound(x: Prefix, y: Prefix): Prefix = match (x, y) {
            case (Bottom, _) => y
            case (_, Bottom) => x
            case (Pre(x1), Pre(y1)) => Pre(String.commonPrefix(x1, y1))
        }
    }

    instance MeetLattice[Prefix] {
        /// /\
        pub def greatestLowerBound(x: Prefix, y: Prefix): Prefix = match (x, y) {
            case (Bottom, _) => Bottom
            case (_, Bottom) => Bottom
            case (Pre(x1), Pre(y1)) => if (x1 <= y1) Pre(x1) else Pre(y1)
        }
    }

}
