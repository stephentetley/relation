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

namespace RelLib/Lattice/Range {

    pub enum Range with Eq, Order, ToString, Boxable {
        case Top
        case Range(Int32, Int32) 
        case Bottom
    }

    pub def range(x: Int32, y: Int32): Range = Range(Int32.min(x, y), Int32.max(x, y))

    pub def inRange(x: Int32, r: Range): Bool = match r {
        case Top         => false
        case Range(m, n) => m <= x and x <= n
        case Bottom      => false
    }


    pub def getValue(x: Range): Option[(Int32, Int32)] = match x {
        case Top         => None
        case Range(m, n) => Some((m, n))
        case Bottom      => None
    }

    
    instance PartialOrder[Range] {
        pub def lessEqual(x: Range, y: Range): Bool = match (x, y) {
            case (Bottom, _)                  => true
            case (Top, Bottom)                => false
            case (Top, Range(_, _))           => false
            case (Top, Top)                   => true
            case (Range(_, _), Bottom)        => false
            case (Range(_, _), Top)           => true
            case (Range(m1, _), Range(m2, _)) => m1 <= m2
        }
    }


    instance LowerBound[Range] {
        pub def minValue(): Range = Bottom
    }

    instance UpperBound[Range] {
        pub def maxValue(): Range = Top
    }

    instance JoinLattice[Range] {
        /// Union
        pub def leastUpperBound(x: Range, y: Range): Range = match (x, y) {
            case (Top, _)                       => Top
            case (Bottom, y1)                   => y1
            case (Range(_, _), Top)             => Top
            case (Range(m1, n1), Bottom)        => Range(m1, n1)
            case (Range(m1, n1), Range(m2, n2)) => Range(Int32.min(m1, m2), Int32.max(n1, n2))
        }
    }

    instance MeetLattice[Range] {
        /// Intersection
        pub def greatestLowerBound(x: Range, y: Range): Range = match (x, y) {
            case (Bottom, _)                    => Bottom
            case (Top, y1)                      => y1
            case (Range(_, _), Bottom)          => Bottom
            case (Range(m1, n1), Top)           => Range(m1, n1)    
            case (Range(m1, n1), Range(m2, n2)) => 
                if (m1 <= m2 and n1 >= m2) 
                    Range(m2, Int32.min(n1, n2))
                else if (m2 <= m1 and n1 >= n2)
                    Range(m1, Int32.min(n1, n2)) 
                else Top
        }
    }

}
