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

#include <meevax/kernel/list.hpp>
#include <meevax/utility/description.hpp>

namespace meevax
{
inline namespace kernel
{
  #define PROCEDURE(...) meevax::object __VA_ARGS__(meevax::pair::const_reference xs)

  struct procedure : public description
  {
    using signature = PROCEDURE((*));

    using applicable = std::function<PROCEDURE()>;

    applicable apply;

    explicit procedure(std::string const&, applicable const&);

    explicit procedure(std::string const&, std::string const&);

    static auto dlopen(std::string const&) -> pointer<void>;

    static auto dlsym(std::string const&, const_pointer<void>) -> signature;
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;

  template <typename T>
  struct is
  {
    auto operator ()(pair::const_reference xs) const -> pair::const_reference
    {
      auto is_T = [](pair::const_reference x)
      {
        return x.is<T>();
      };

      return std::all_of(std::begin(xs), std::end(xs), is_T) ? t : f;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
