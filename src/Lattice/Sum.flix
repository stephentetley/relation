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

pub enum Sum[a] with Eq, Order, ToString, Boxable { case Sum(a) }

instance PartialOrder[Sum[a]] with Order[a] {
    pub def lessEqual(x: Sum[a], y: Sum[a]): Bool = 
        let Sum(x1) = x;
        let Sum(y1) = y;
        x1 <= y1
}

instance LowerBound[Sum[a]] with LowerBound[a] {
    pub def minValue(): Sum[a] = Sum(LowerBound.minValue())
}

instance MeetLattice[Sum[a]] with Add[a] {
    /// /\
    pub def greatestLowerBound(x: Sum[a], y: Sum[a]): Sum[a] = 
        let Sum(x1) = x;
        let Sum(y1) = y;
        Sum(x1 + y1)
}

instance JoinLattice[Sum[a]] with Add[a] {
    /// \/
    pub def leastUpperBound(x: Sum[a], y: Sum[a]): Sum[a] = 
        let Sum(x1) = x;
        let Sum(y1) = y;
        Sum(x1 + y1)
}

