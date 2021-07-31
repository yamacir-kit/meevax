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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  struct syntactic_keyword : public virtual pair
  {
    using pair::pair;

    auto unwrap_syntax() const noexcept -> let const&
    {
      return std::get<0>(*this);
    }

    auto global_environment() const noexcept -> let const&
    {
      return std::get<1>(*this);
    }

    auto assq() const -> decltype(auto)
    {
      return kernel::assq(unwrap_syntax(), global_environment());
    }

    let const& lookup() const
    {
      if (let const& x = assq(); x != f)
      {
        return cdr(x);
      }
      else
      {
        return unwrap_syntax();
      }
    }

    auto is_free() const
    {
      return assq().eqv(f);
    }

    auto is_bound() const
    {
      return not is_free();
    }

    friend auto operator <<(output_port & port, syntactic_keyword const& datum) -> output_port &
    {
      return port << underline << datum.unwrap_syntax() << reset;
    }
  };

  auto lookup(let const& x, let const& g) -> let const&
  {
    if (let const& p = assq(x, g); p != f)
    {
      return cdr(p);
    }
    else
    {
      return x.is<syntactic_keyword>() ? x.as<syntactic_keyword>().lookup() : x;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP
