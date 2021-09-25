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

#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  #define PROCEDURE(...) meevax::let __VA_ARGS__(meevax::let const& xs)

  struct procedure : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    std::string const name;

    explicit procedure(std::string const&, std::function<PROCEDURE()> const&);

    explicit procedure(std::string const& name, std::string const& libfoo_so);

    virtual ~procedure() = default;

    auto load(std::string const&, pointer<void> const&) -> signature;

    auto open(std::string const&) -> pointer<void>;
  };

  auto operator <<(std::ostream & port, procedure const& datum) -> std::ostream &;

  template <typename T>
  struct is
  {
    let const& operator ()(let const& xs) const
    {
      auto is_T = [](let const& x)
      {
        return x.is<T>();
      };

      return std::all_of(std::begin(xs), std::end(xs), is_T) ? t : f;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
