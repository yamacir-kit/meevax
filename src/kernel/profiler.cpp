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

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <vector>

#include <meevax/kernel/profiler.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace kernel
{
  struct column : public topic
  {
    std::string name;

    template <typename... Ts>
    explicit column(std::type_index const& id, Ts&&... xs)
      : topic { std::forward<decltype(xs)>(xs)... }
      , name  { demangle(id.name()) }
    {}
  };

  profiler::~profiler()
  {
    auto make_table = [this]()
    {
      std::vector<column> result;

      for (auto const& [key, value] : *this)
      {
        result.emplace_back(key, value);
      }

      return result;
    };

    auto table = make_table();

    std::sort(std::begin(table), std::end(table), [](auto&& a, auto&& b)
    {
      return a.allocation > b.allocation;
    });

    std::vector<std::size_t> column_width;

    column_width.push_back(std::max_element(std::begin(table), std::end(table), [](auto&& a, auto&& b) { return a.name.length() < b.name.length(); })->name.length());
    column_width.push_back(12);

    std::cout << std::string(80, '-') << "\n"
              << std::left  << std::setw(column_width[0]) << "Typename"
              << std::right << std::setw(column_width[1]) << "Allocations"
              << "\n"
              << std::string(80, '-') << "\n";

    for (auto const& each : table)
    {
      std::cout << std::left  << std::setw(column_width[0]) << each.name
                << std::right << std::setw(column_width[1]) << each.allocation
                << std::endl;
    }

    std::cout << std::string(80, '-') << "\n"
              << std::left  << std::setw(column_width[0]) << "Total"
              << std::right << std::setw(column_width[1]) << std::accumulate(std::begin(table), std::end(table), 0, [](auto&& a, auto&& b) { return a + b.allocation; })
              << "\n"
              << std::string(80, '-') << "\n";
  }

  auto current_profiler() -> profiler &
  {
    static profiler value {};
    return value;
  }
} // namespace kernel
} // namespace meevax
