/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <exception>
#include <tuple>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/control.hpp>
#include <meevax/kernel/dynamic_environment.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax::inline kernel
{
  auto dynamic_environment::apply(object const& f, object const& xs) -> object
  {
    let c = make<vector>();
    c.as<vector>().objects.emplace_back(instruction::call);
    return execute(cons(f, make<continuation>(), xs), nullptr, c);
  }

  auto dynamic_environment::execute(object const& c) -> object
  {
    return execute(nullptr, nullptr, c);
  }

  auto dynamic_environment::execute(object s, object e, object c) -> object
  {
    try
    {
      let * c0 = c.as<vector>().objects.data();
      let * pc = c0;

    fetch:
      switch ((pc++)->as<instruction>())
      {
      case instruction::load_absolute:
        assert(pc->is_also<absolute>());
        s = std::forward_as_tuple(cdr(*pc), s);
        ++pc;
        goto fetch;

      case instruction::load_relative:
        assert(pc->is_also<relative>());
        s = std::forward_as_tuple(head(head(e, static_cast<std::size_t>(car(*pc).as<small_integer>())), static_cast<std::size_t>(cdr(*pc).as<small_integer>())), s);
        ++pc;
        goto fetch;

      case instruction::load_variadic:
        assert(pc->is_also<variadic>());
        s = std::forward_as_tuple(tail(head(e, static_cast<std::size_t>(car(*pc).as<small_integer>())), static_cast<std::size_t>(cdr(*pc).as<small_integer>())), s);
        ++pc;
        goto fetch;

      case instruction::load_constant:
        s = std::forward_as_tuple(*pc, s);
        ++pc;
        goto fetch;

      case instruction::load_closure:
        assert(pc->is<control>());
        s = std::forward_as_tuple(make<closure, segregated_storage_allocator<void>>(*pc, e), s);
        ++pc;
        goto fetch;

      case instruction::current:
        s = std::forward_as_tuple(a[static_cast<std::size_t>(pc->as<small_integer>())], s);
        ++pc;
        goto fetch;

      case instruction::select:
        assert(pc[0].is<control>());
        assert(pc[1].is<control>());
        pc = c0 + cdr(car(s) != f ? pc[0] : pc[1]).as<small_integer>();
        s = cdr(s);
        goto fetch;

      case instruction::call:
        if (let const& f = car(s); f.is<closure>())
        {
          assert(car(f).is<control>());

          e = std::forward_as_tuple(cdr(s), cdr(f));
          s = nullptr;

          if (c != caar(f))
          {
            c = caar(f);
            c0 = c.as<vector>().objects.data();
          }

          pc = c0 + static_cast<std::size_t>(cdar(f).as<small_integer>());
          goto fetch;
        }
        else if (f.is<procedure>())
        {
          if (let const& k = cadr(s); k.is<closure>())
          {
            assert(car(k).is<control>());

            e = std::forward_as_tuple(list(f.as<procedure>().call(cddr(s))), cdr(k));
            s = nullptr;

            if (c != caar(k))
            {
              c = caar(k);
              c0 = c.as<vector>().objects.data();
            }

            pc = c0 + static_cast<std::size_t>(cdar(k).as<small_integer>());
            goto fetch;
          }
          else if (k.is<continuation>())
          {
            return f.as<procedure>().call(cddr(s));
          }
          else
          {
            throw error(make<string>("not applicable"), k);
          }
        }
        else if (f.is<continuation>())
        {
          return cadr(s);
        }
        else
        {
          throw error(make<string>("not applicable"), f);
        }

      case instruction::drop:
        s = cdr(s);
        goto fetch;

      case instruction::store_absolute:
        assert(pc->is<absolute>());
        cdr(*pc) = car(s);
        ++pc;
        goto fetch;

      case instruction::store_relative:
        assert(pc->is<relative>());
        head(head(e, static_cast<std::size_t>(car(*pc).as<small_integer>())), static_cast<std::size_t>(cdr(*pc).as<small_integer>())) = car(s);
        ++pc;
        goto fetch;

      case instruction::store_variadic:
        assert(pc->is<variadic>());
        tail(head(e, static_cast<std::size_t>(car(*pc).as<small_integer>())), static_cast<std::size_t>(cdr(*pc).as<small_integer>())) = car(s);
        ++pc;
        goto fetch;

      case instruction::install:
        a[static_cast<std::size_t>(pc->as<small_integer>())] = car(s);
        ++pc;
        goto fetch;

      case instruction::list_values:
        s = car(s);
        goto fetch;

      default:
        assert(false);
        std::terminate();
      }
    }
    catch (object const& thrown) // by the procedure `throw`.
    {
      if (thrown.is_also<error>())
      {
        thrown.as<error>().raise();
        return unspecified;
      }
      else
      {
        throw error(make<string>("uncaught exception"), thrown);
      }
    }
    catch (error & thrown) // by any procedure other than `throw`.
    {
      if (exception_handler)
      {
        return apply(exception_handler, list(thrown.make()));
      }
      else // In most cases, this clause will never be called.
      {
        thrown.raise();
        return unspecified;
      }
    }
    catch (std::exception const& exception) // by the system.
    {
      if (auto thrown = error(make<string>(exception.what()), unit); exception_handler)
      {
        return apply(exception_handler, list(thrown.make()));
      }
      else // In most cases, this clause will never be called.
      {
        throw thrown;
      }
    }
  }
} // namespace meevax::kernel
