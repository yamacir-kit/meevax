/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_PROFILER_HPP
#define INCLUDED_MEEVAX_KERNEL_PROFILER_HPP

#include <typeindex>
#include <unordered_map>

#include <meevax/kernel/instruction.hpp>

namespace meevax
{
inline namespace kernel
{
  struct profiler
  {
    static constexpr auto count_allocations       = false;
    static constexpr auto count_instruction_fetch = false;

    std::unordered_map<std::type_index, std::size_t> allocation_counts;

    std::unordered_map<instruction, std::size_t> instruction_fetchs;

    ~profiler();
  };

  auto current_profiler() -> profiler &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROFILER_HPP
