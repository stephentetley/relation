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

namespace RelLib/Lattice/Constant {

    use RelLib/Lattice/Trans/Wide.{Wide};
    use RelLib/Lattice/Trans/Wide.Wide.{Top, Middle, Bottom};

    /// TODO - not sure we should be using Wide - must check...

    pub enum Constant[a] with Eq, Order, ToString, Boxable { case Constant(Wide[a]) }

    pub def constant(x: a): Constant[a] = Constant(Middle(x))
    
    instance Functor[Constant] {
        pub def map(f: a -> b & ef, x: Constant[a]): Constant[b] & ef = match x {
            case Constant(a) => Constant(Functor.map(f, a))
        }
    }

    instance PartialOrder[Constant[a]] with Eq[a] {
        pub def lessEqual(x: Constant[a], y: Constant[a]): Bool = match (x, y) { 
            case (Constant(x1), Constant(y1)) => PartialOrder.lessEqual(x1, y1)
        }
    }

    instance LowerBound[Constant[a]] {
        pub def minValue(): Constant[a] = Constant(LowerBound.minValue())
    }

    instance UpperBound[Constant[a]] {
        pub def maxValue(): Constant[a] = Constant(UpperBound.maxValue())
    }
    
    instance JoinLattice[Constant[a]] with Eq[a] {
        /// \/
        pub def leastUpperBound(x: Constant[a], y: Constant[a]): Constant[a] = match (x, y) { 
            case (Constant(x1), Constant(y1)) => Constant(JoinLattice.leastUpperBound(x1, y1))
        }
    }

    instance MeetLattice[Constant[a]] with Eq[a] {
        /// /\
        pub def greatestLowerBound(x: Constant[a], y: Constant[a]): Constant[a] = match (x, y) { 
            case (Constant(x1), Constant(y1)) => Constant(MeetLattice.greatestLowerBound(x1, y1))
        }
    }


}