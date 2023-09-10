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

#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct procedure
  {
    std::string const name;

    explicit procedure(std::string const& name)
      : name { name }
    {}

    virtual auto operator ()(object & = unit) const -> object = 0;
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;

  #define FUNCTION(...) auto __VA_ARGS__(meevax::object const& xs) -> meevax::object

  struct function : public procedure
  {
    auto (*call)(object const&) -> object;

    explicit function(std::string const& name, auto (*call)(object const&) -> object)
      : procedure { name }
      , call { call }
    {}

    explicit function(std::string const&, std::string const&);

    auto operator ()(object & xs) const -> object override
    {
      return call(xs);
    }
  };

  struct function_object : public procedure
  {
    std::function<FUNCTION()> const call;

    explicit function_object(std::string const& name, std::function<FUNCTION()> const& call)
      : procedure { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return call(xs);
    }
  };

  struct accessor : public procedure
  {
    auto (*call)(object const&) -> object const&;

    explicit accessor(std::string const& name, auto (*call)(object const&) -> object const&)
      : procedure { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return call(xs);
    }
  };

  struct predicate : public procedure
  {
    auto (*call)(object const&) -> bool;

    explicit predicate(std::string const& name, auto (*call)(object const&) -> bool)
      : procedure { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return call(xs) ? t : f;
    }
  };

  struct modifier : public procedure // mutation-procedure
  {
    auto (*call)(object &) -> void;

    explicit modifier(std::string const& name, auto (*call)(object &) -> void)
      : procedure { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      call(xs);
      return unspecified;
    }
  };

  struct command : public procedure
  {
    auto (*call)(object const&) -> void;

    explicit command(std::string const& name, auto (*call)(object const&) -> void)
      : procedure { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      call(xs);
      return unspecified;
    }
  };

  struct thunk : public procedure
  {
    auto (*call)() -> object;

    explicit thunk(std::string const& name, auto (*call)() -> object)
      : procedure { name }
      , call { call }
    {}

    auto operator ()(object &) const -> object override
    {
      return call();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
