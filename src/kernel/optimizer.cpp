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

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/optimizer.hpp>

namespace meevax::inline kernel
{
  struct analysis
  {
    static constexpr auto n = 23;

    std::size_t adjacencies[n][n] = {};

    ~analysis()
    {
      constexpr std::size_t width = 18;

      std::cerr << std::string(width, ' ');

      for (auto i = 0; i < n; ++i)
      {
        std::cerr << std::setw(width) << std::right << static_cast<instruction>(i);
      }

      std::cerr << std::endl;

      for (auto i = 0; i < n; ++i)
      {
        std::cerr << std::setw(width) << std::right << static_cast<instruction>(i);

        for (auto j = 0; j < n; ++j)
        {
          std::cerr << std::setw(width) << std::right << adjacencies[i][j];
        }

        std::cerr << std::endl;
      }
    }

    auto adjacency(object const& i, object const& j) -> decltype(auto)
    {
      return adjacencies[static_cast<std::underlying_type_t<instruction>>(i.as<instruction>())]
                        [static_cast<std::underlying_type_t<instruction>>(j.as<instruction>())];
    }
  };

  auto analyze(object const& c) -> void
  {
    assert(c.is<pair>());

    assert(car(c).is<instruction>());

    static auto a = analysis();

    switch (car(c).as<instruction>())
    {
    case instruction::join:
    case instruction::tail_call:
    case instruction::tail_letrec:
    case instruction::return_:
    case instruction::stop:
      assert(cdr(c).is<null>());
      break;

    case instruction::call:
    case instruction::cons:
    case instruction::drop:
    case instruction::dummy:
    case instruction::letrec:
      analyze(cdr(c));
      a.adjacency(car(c), cadr(c))++;
      break;

    case instruction::current:
    case instruction::install:
    case instruction::load_absolute:
    case instruction::load_constant:
    case instruction::load_relative:
    case instruction::load_variadic:
    case instruction::store_absolute:
    case instruction::store_relative:
    case instruction::store_variadic:
      analyze(cddr(c));
      a.adjacency(car(c), caddr(c))++;
      break;

    case instruction::load_closure:
    case instruction::load_continuation:
      analyze(cadr(c));
      analyze(cddr(c));
      a.adjacency(car(c), caddr(c))++;
      break;

    case instruction::select:
      analyze(cadr(c));
      analyze(caddr(c));
      analyze(cdddr(c));
      a.adjacency(car(c), cadddr(c))++;
      break;

    case instruction::tail_select:
      assert(cdddr(c).is<null>());
      analyze(cadr(c));
      analyze(caddr(c));
      break;
    }
  }

  auto merge_constants(object & c) -> void
  {
    assert(c.is<pair>());

    assert(car(c).is<instruction>());

    switch (car(c).as<instruction>())
    {
    case instruction::load_constant: /* ----------------------------------------
    *
    *  (load-constant x
    *   load-constant y
    *   cons
    *   ...)
    *
    *  => (load-constant (y . x)
    *      ...)
    *
    * ----------------------------------------------------------------------- */
      if (cddr(c).is<pair>() and
          caddr(c).is<instruction>() and caddr(c).as<instruction>() == instruction::load_constant and
          cddddr(c).is<pair>() and
          caddddr(c).is<instruction>() and caddddr(c).as<instruction>() == instruction::cons)
      {
        cadr(c) = cons(cadddr(c), cadr(c));
        cddr(c) = cdddddr(c);
        merge_constants(c);
      }
      else
      {
        merge_constants(cddr(c));
      }
      break;

    case instruction::join:
    case instruction::tail_call:
    case instruction::tail_letrec:
    case instruction::return_:
    case instruction::stop:
      assert(cdr(c).is<null>());
      break;

    case instruction::call:
    case instruction::cons:
    case instruction::drop:
    case instruction::dummy:
    case instruction::letrec:
      merge_constants(cdr(c));
      break;

    case instruction::current:
    case instruction::install:
    case instruction::load_absolute:
    case instruction::load_relative:
    case instruction::load_variadic:
    case instruction::store_absolute:
    case instruction::store_relative:
    case instruction::store_variadic:
      merge_constants(cddr(c));
      break;

    case instruction::load_closure:
    case instruction::load_continuation:
      merge_constants(cadr(c));
      merge_constants(cddr(c));
      break;

    case instruction::select:
      merge_constants(cadr(c));
      merge_constants(caddr(c));
      merge_constants(cdddr(c));
      break;

    case instruction::tail_select:
      assert(cdddr(c).is<null>());
      merge_constants(cadr(c));
      merge_constants(caddr(c));
      break;
    }
  }

  auto optimize(object code) -> object
  {
    merge_constants(code);

    if constexpr (false)
    {
      analyze(code);
    }

    return code;
  }
} // namespace meevax::kernel
