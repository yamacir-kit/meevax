/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

#include <meevax/kernel/notation.hpp>

namespace meevax
{
inline namespace kernel
{
  struct syntactic_closure : public virtual pair // (<notation> . e)
  {
    using pair::pair;

    auto symbol() const -> const_reference
    {
      assert(first.is_also<notation>());
      return first.as<notation>().symbol();
    }

    auto strip()
    {
      assert(first.is_also<notation>());
      return first.as<notation>().strip(second);
    }

    auto is_bound() const -> bool
    {
      return not is_free();
    }

    auto is_free() const -> bool
    {
      assert(first.is_also<notation>());
      return first.is<absolute>() and first.as<absolute>().is_free();
    }
  };

  auto notate(const_reference, const_reference) -> object;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
