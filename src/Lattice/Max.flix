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

namespace RelLib/Lattice/Max {

    use RelLib/Lattice/Ordered.{Ordered};
    use RelLib/Lattice/Ordered.Ordered.{Ordered};

    pub enum Max[a] with Eq, Order, ToString, Boxable { case Max(Ordered[a]) }

    pub def max1(x: a): Max[a] = Max(Ordered(x))

    pub def getValue(x: Max[a]): a = match x {
        case Max(Ordered(x1)) => x1
    }

    instance Functor[Max] {
        pub def map(f: a -> b & ef, x: Max[a]): Max[b] & ef = match x {
            case Max(a) => Max(Functor.map(f, a))
        }
    }

    instance PartialOrder[Max[a]] with Order[a] {
        pub def lessEqual(x: Max[a], y: Max[a]): Bool = match (x, y) {
            case (Max(x1), Max(y1)) => PartialOrder.lessEqual(x1, y1)
        }
    }

    instance LowerBound[Max[a]] with LowerBound[a] {
        pub def minValue(): Max[a] = Max(LowerBound.minValue())
    }

    instance UpperBound[Max[a]] with UpperBound[a] {
        pub def maxValue(): Max[a] = Max(UpperBound.maxValue())
    }


    instance JoinLattice[Max[a]] with Order[a] {
        /// \/
        pub def leastUpperBound(x: Max[a], y: Max[a]): Max[a] = 
            match (x, y) {
                case (Max(x1), Max(y1)) => Max(JoinLattice.leastUpperBound(x1, y1))
            }
    }

    instance MeetLattice[Max[a]] with Order[a] {
        /// /\
        pub def greatestLowerBound(x: Max[a], y: Max[a]): Max[a] = 
            match (x, y) {
                case (Max(x1), Max(y1)) => Max(MeetLattice.greatestLowerBound(x1, y1))
            }
    }

}
