/*
 * Copyright 2022 Stephen Tetley
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

namespace RelLib/Lattice/Boolean {

    use RelLib/Lattice/Trans/Ordered.{Ordered};
    use RelLib/Lattice/Trans/Ordered.Ordered.{Ordered};

    pub enum Boolean with Eq, Order, ToString, Boxable {
        case Bottom
        case Boolean(Bool)
        case Top
    }


    pub def boolean(b: Bool): Boolean = Boolean(b)

    pub def getValue(x: Boolean): Option[Bool] = match x {
        case Boolean(x1) => Some(x1)
        case _           => None
    }


    instance PartialOrder[Boolean] {
        pub def lessEqual(x: Boolean, y: Boolean): Bool = match (x, y) {
            case (Bottom, _)                => true
            case (Boolean(x1), Boolean(x2)) => x1 == x2
            case (_, Top)                   => true
            case _                          => false
        }
    }

    instance LowerBound[Boolean] {
        pub def minValue(): Boolean = Bottom
    }

    instance UpperBound[Boolean] {
        pub def maxValue(): Boolean = Top
    }

    instance JoinLattice[Boolean] {
        /// \/ boolean OR
        pub def leastUpperBound(x: Boolean, y: Boolean): Boolean = match (x, y) {
            case (Bottom, _)                => y
            case (_, Bottom)                => x
            case (Boolean(x1), Boolean(y1)) => Boolean(x1 or y1)
            case _                          => Top
        }
    }

    instance MeetLattice[Boolean] {
        /// /\ boolean AND
        pub def greatestLowerBound(x: Boolean, y: Boolean): Boolean = match (x, y) {
            case (Top, _)                   => y
            case (_, Top)                   => x
            case (Boolean(x1), Boolean(y1)) => Boolean(x1 and y1)
            case _                          => Bottom
        }
    }


}
