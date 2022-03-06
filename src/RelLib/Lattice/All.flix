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

namespace RelLib/Lattice/All {



    pub enum All with Eq, Order, ToString, Boxable { 
        /// case Top
        case All(Bool)
        case Bottom
    }




    instance PartialOrder[All] {
        pub def lessEqual(x: All, y: All): Bool = match (x, y) { 
            case (Bottom, _) => true
            /// case (Top, Bottom) => false
            /// case (Top, All(_)) => false
            /// case (Top, Top) => true
            case (All(_), Bottom) => false
            /// case (All(_), Top) => true
            case (All(x1), All(y1)) => x1 == y1
        }
    }

    instance LowerBound[All] {
        pub def minValue(): All = Bottom
    }


    instance UpperBound[All] {
        pub def maxValue(): All = All(true)     /// Top
    }

    instance JoinLattice[All] {
        /// \/
        pub def leastUpperBound(x: All, y: All): All = match (x, y) {
            /// case (Top, _) => Top
            case (Bottom, y1) => y1
            /// case (All(_), Top) => Top
            case (All(x1), Bottom) => All(x1)
            case (All(x1), All(y1)) => if (x1 and y1) All(true) else All(false)     /// else All(Top)
        }
    }

    instance MeetLattice[All] {
        /// /\
        pub def greatestLowerBound(x: All, y: All): All = match (x, y) {
            case (Bottom, _) => Bottom
            /// case (Top, y1) => y1
            case (All(_), Bottom) => Bottom
            /// case (All(x1), Top) => All(x1)    
            case (All(x1), All(y1)) => if (x1 and y1) All(true) else Bottom
        }
    }

}
