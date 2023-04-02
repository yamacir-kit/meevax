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

#ifndef INCLUDED_MEEVAX_MEMORY_MARKER_HPP
#define INCLUDED_MEEVAX_MEMORY_MARKER_HPP

namespace meevax
{
inline namespace memory
{
  class marker
  {
    static inline bool phase;

    bool value;

  public:
    struct initializer
    {
      explicit initializer();
    };

    explicit marker() noexcept
      : value { phase }
    {}

    auto mark() noexcept
    {
      value = phase;
    }

    auto marked() const noexcept
    {
      return value == phase;
    }

    static auto toggle() noexcept
    {
      return phase = not phase;
    }
  };

  static marker::initializer initializer;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_MARKER_HPP
