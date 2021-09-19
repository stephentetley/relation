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

namespace RelLib {

    type alias RegexExtractor[r] = 
        { pattern: Text/Regex.Pattern
        , evaluator: Text/Regex.MatchEvaluator[r]
        }

}

namespace RelLib/RegexExtractor {

    use Text/Regex.{Pattern};
    use RelLib.{RegexExtractor};

    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Throws an error, crashing the program if the regex is ill-formed and cannot be compiled.
    ///
    /// Use this only for scripts that are expected to "fail fast"..
    ///
    pub def unsafeCompileRegex(regex: String): Pattern =
        match Text/Regex/Pattern.compile(regex) { 
            case Ok(patt) => patt
            case Err(_) => bug!("RelLib/RegexExtractor.unsafeCompileRegex failed - '${regex}'")
        }
        

    /// uses `contains`...
    pub def extract(extract: RegexExtractor[#{|r}], input: String): #{|r} & Impure =  
        /// let patt   = match Text/Regex/Pattern.compile(extract.regex) {
        ///     case Err(err) => bug!(err)
        ///     case Ok(p1) => p1
        /// };
        match Text/Regex.evalContains(input, extract.pattern, extract.evaluator) {
            case Err(_) => #{}
            case Ok(r1) => r1
        }

}