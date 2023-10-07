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
  struct callable
  {
    std::string const name;

    explicit callable(std::string const& name)
      : name { name }
    {}

    virtual auto operator ()(object & = unit) const -> object = 0;
  };

  auto operator <<(std::ostream &, callable const&) -> std::ostream &;

  struct procedure : public callable
  {
    auto (*call)(object const&) -> object;

    explicit procedure(std::string const& name, auto (*call)(object const&) -> object)
      : callable { name }
      , call { call }
    {}

    explicit procedure(std::string const&, std::string const&);

    auto operator ()(object & xs) const -> object override
    {
      return call(xs);
    }
  };

  struct functor : public callable
  {
    #define FUNCTION(...) auto __VA_ARGS__(meevax::object const& xs) -> meevax::object

    std::function<FUNCTION()> const call;

    explicit functor(std::string const& name, std::function<FUNCTION()> const& call)
      : callable { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return call(xs);
    }
  };

  struct accessor : public callable
  {
    auto (*call)(object const&) -> object const&;

    explicit accessor(std::string const& name, auto (*call)(object const&) -> object const&)
      : callable { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return call(xs);
    }
  };

  struct predicate : public callable
  {
    auto (*call)(object const&) -> bool;

    explicit predicate(std::string const& name, auto (*call)(object const&) -> bool)
      : callable { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return call(xs) ? t : f;
    }
  };

  struct mutation : public callable
  {
    auto (*call)(object &) -> void;

    explicit mutation(std::string const& name, auto (*call)(object &) -> void)
      : callable { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      call(xs);
      return unspecified;
    }
  };

  struct command : public callable
  {
    auto (*call)(object const&) -> void;

    explicit command(std::string const& name, auto (*call)(object const&) -> void)
      : callable { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      call(xs);
      return unspecified;
    }
  };

  struct thunk : public callable
  {
    auto (*call)() -> object;

    explicit thunk(std::string const& name, auto (*call)() -> object)
      : callable { name }
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
