/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <iomanip>

#include <meevax/chrono/duration.hpp>

namespace meevax::inline chrono
{
  auto operator <<(std::ostream & os, std::chrono::nanoseconds nanoseconds) -> std::ostream &
  {
    auto const fill = os.fill();

    os.fill('0');

    auto const years = std::chrono::duration_cast<meevax::years>(nanoseconds);

    auto const months = std::chrono::duration_cast<meevax::months>(nanoseconds -= years);

    auto const days = std::chrono::duration_cast<meevax::days>(nanoseconds -= months);

    auto const hours = std::chrono::duration_cast<std::chrono::hours>(nanoseconds -= days);

    auto const minutes = std::chrono::duration_cast<std::chrono::minutes>(nanoseconds -= hours);

    auto const seconds = std::chrono::duration_cast<std::chrono::seconds>(nanoseconds -= minutes);

    nanoseconds -= seconds;

    os << 'P'
       << std::setw(4) <<       years.count() << '-'
       << std::setw(2) <<      months.count() << '-'
       << std::setw(2) <<        days.count() << 'T'
       << std::setw(2) <<       hours.count() << ':'
       << std::setw(2) <<     minutes.count() << ':'
       << std::setw(2) <<     seconds.count() << '.'
       << std::setw(9) << nanoseconds.count();

    os.fill(fill);

    return os;
  }
} // namespace meevax::chrono
