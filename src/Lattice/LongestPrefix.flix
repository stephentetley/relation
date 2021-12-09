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

namespace RelLib/Lattice/LongestPrefix {

    /// TODO this is really `LongestString`
    /// There's nothing mandating the String is a prefix
        
    pub enum LongestPrefix with Eq, Order, ToString, Boxable {
        case Top
        case LongestPrefix(String) 
        case Bottom
    }

    pub def longestPrefix(x: String): LongestPrefix = LongestPrefix(x)

    pub def getValue(x: LongestPrefix): Option[String] = match x {
        case Top               => None
        case LongestPrefix(s1) => Some(s1)
        case Bottom            => None
    }

    /// This might be the problem...
    instance PartialOrder[LongestPrefix] {
        pub def lessEqual(x: LongestPrefix, y: LongestPrefix): Bool = match (x, y) {
            case (Bottom, _)                            => true
            case (Top, Bottom)                          => false
            case (Top, LongestPrefix(_))                => false
            case (Top, Top)                             => true
            case (LongestPrefix(_), Bottom)             => false
            case (LongestPrefix(_), Top)                => true
            case (LongestPrefix(x1), LongestPrefix(y1)) => x1 <= y1
        }
    }


    instance LowerBound[LongestPrefix] {
        pub def minValue(): LongestPrefix = Bottom
    }

    instance UpperBound[LongestPrefix] {
        pub def maxValue(): LongestPrefix = Top
    }

    instance JoinLattice[LongestPrefix] {
        /// \/
        pub def leastUpperBound(x: LongestPrefix, y: LongestPrefix): LongestPrefix = match (x, y) {
            case (Top, _)                               => Top
            case (Bottom, y1)                           => y1
            case (LongestPrefix(_), Top)                => Top
            case (LongestPrefix(x1), Bottom)            => LongestPrefix(x1)
            case (LongestPrefix(x1), LongestPrefix(y1)) => 
                if (String.length(x1) <= String.length(y1)) LongestPrefix(y1) else LongestPrefix(x1)
        }
    }

    instance MeetLattice[LongestPrefix] {
        /// /\
        pub def greatestLowerBound(x: LongestPrefix, y: LongestPrefix): LongestPrefix = match (x, y) {
            case (Bottom, _)                            => Bottom
            case (Top, y1)                              => y1
            case (LongestPrefix(_), Bottom)             => Bottom
            case (LongestPrefix(x1), Top)               => LongestPrefix(x1)    
            case (LongestPrefix(x1), LongestPrefix(y1)) => 
                if (String.length(x1) <= String.length(y1)) LongestPrefix(x1) else Bottom
        }
    }

}
