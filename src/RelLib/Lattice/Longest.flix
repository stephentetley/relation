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

namespace RelLib/Lattice/Longest {

    /// TODO is this really a newtype and Ordered?


    pub enum Longest[a] with Eq, Order, ToString, Boxable {
        case Top
        case Longest(a) 
        case Bottom
    }

    pub class LongerThan[a] {
        pub def longerThan(x: a, y: a): Bool
    }

    instance LongerThan[String] {
        pub def longerThan(x: String, y: String): Bool = 
            String.length(x) > String.length(y)
    }


    pub def longest(x: a): Longest[a] with LongerThan[a] = Longest(x)


    pub def longest2(x: a, y: a): Longest[a] with LongerThan[a] = if (y `RelLib/Lattice/Longest/LongerThan.longerThan` x) Longest(y) else Longest(x)

    pub def getValue(x: Longest[a]): Option[a] = match x {
        case Top         => None
        case Longest(x1) => Some(x1)
        case Bottom      => None
    }

    /// TODO instances need checking...
    
    instance PartialOrder[Longest[a]] with LongerThan[a] {        
        pub def lessEqual(x: Longest[a], y: Longest[a]): Bool = match (x, y) {
            case (Bottom, _)                => true
            case (Top, Bottom)              => false
            case (Top, Longest(_))          => false
            case (Top, Top)                 => true
            case (Longest(_), Bottom)       => false
            case (Longest(_), Top)          => true
            case (Longest(x1), Longest(y1)) => RelLib/Lattice/Longest/LongerThan.longerThan(y1, x1)
        }
    }


    instance LowerBound[Longest[a]] {
        pub def minValue(): Longest[a] = Bottom
    }

    instance UpperBound[Longest[a]] {
        pub def maxValue(): Longest[a] = Top
    }

    instance JoinLattice[Longest[a]] with LongerThan[a] {
        /// \/ 'longer than'
        pub def leastUpperBound(x: Longest[a], y: Longest[a]): Longest[a] = match (x, y) {
            case (Top, _)                   => Top
            case (Bottom, y1)               => y1
            case (Longest(_), Top)          => Top
            case (Longest(x1), Bottom)      => Longest(x1)
            case (Longest(x1), Longest(y1)) => if (y1 `RelLib/Lattice/Longest/LongerThan.longerThan` x1) Longest(x1) else Longest(x1)
        }
    }

    instance MeetLattice[Longest[a]] with LongerThan[a] {
        /// /\
        pub def greatestLowerBound(x: Longest[a], y: Longest[a]): Longest[a] = match (x, y) {
            case (Bottom, _)                => Bottom
            case (Top, y1)                  => y1
            case (Longest(_), Bottom)       => Bottom
            case (Longest(x1), Top)         => Longest(x1)    
            case (Longest(x1), Longest(y1)) => if (y1 `RelLib/Lattice/Longest/LongerThan.longerThan` x1) Longest(x1) else Bottom
        }
    }

}
