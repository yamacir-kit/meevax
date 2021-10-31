/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_STANDARD_HPP
#define INCLUDED_MEEVAX_STANDARD_HPP

#include <meevax/kernel/environment.hpp>

namespace meevax
{
  #define DEFINE_LIBRARY(NAME)                                                 \
  namespace standard                                                           \
  {                                                                            \
    struct NAME##_t                                                            \
    {                                                                          \
      explicit NAME##_t() = default;                                           \
    }                                                                          \
    inline constexpr NAME {};                                                  \
  }                                                                            \
                                                                               \
  template <>                                                                  \
  auto environment::import(standard::NAME##_t) -> void

  DEFINE_LIBRARY(base);
  // DEFINE_LIBRARY(case_lambda);
  DEFINE_LIBRARY(character);
  // DEFINE_LIBRARY(complex);
  DEFINE_LIBRARY(cxr);
  DEFINE_LIBRARY(evaluate);
  // DEFINE_LIBRARY(file);
  DEFINE_LIBRARY(inexact);
  // DEFINE_LIBRARY(lazy);
  DEFINE_LIBRARY(load);
  DEFINE_LIBRARY(process_context);
  // DEFINE_LIBRARY(r5rs);
  DEFINE_LIBRARY(read);
  // DEFINE_LIBRARY(repl);
  // DEFINE_LIBRARY(time);
  DEFINE_LIBRARY(write);

  DEFINE_LIBRARY(experimental);
  DEFINE_LIBRARY(srfis);

  DEFINE_LIBRARY(interaction_environment);
} // namespace meevax

#endif // INCLUDED_MEEVAX_STANDARD_HPP
