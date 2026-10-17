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
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
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
#if 0
    return execute(cons(f, make<continuation>(), xs),
                   nullptr,
                   list(make<instruction>(instruction::call)));
#else
    let c = make<vector>();
    c.as<vector>().objects.emplace_back(instruction::call);
    return execute(cons(f, make<continuation>(), xs), nullptr, c);
#endif
  }

  auto dynamic_environment::execute(object const& c) -> object
  {
    return execute(nullptr, nullptr, c);
  }

  auto dynamic_environment::execute(object s, object e, object c) -> object
  {
#if 0
    auto i = [&]() -> decltype(auto)
    {
      assert(cadr(c).is<relative>() or cadr(c).is<variadic>());
      assert(car(cadr(c)).is<small_integer>());
      assert(car(cadr(c)).as<small_integer>() < length(e));
      return car(cadr(c)).as<small_integer>();
    };

    auto j = [&]() -> decltype(auto)
    {
      assert(cadr(c).is<relative>() or cadr(c).is<variadic>());
      assert(cdr(cadr(c)).is<small_integer>());
      return cdr(cadr(c)).as<small_integer>();
    };

    try
    {
    fetch:
      assert(c);

      switch (car(c).template as<instruction>())
      {
      case instruction::load_absolute: /* --------------------------------------
        *
        *  s e (%load-absolute <absolute> . c) => (x . s) e c
        *
        *  where <absolute> = (<symbol> . x)
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is_also<absolute>());
        s.reset<bx, b1>(cons(cdadr(c), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::load_relative: /* --------------------------------------
        *
        *  s  e (%load-relative <relative> . c) => (x . s) e c
        *
        *  where <relative> = (i . j)
        *
        *        x = (list-ref (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(head(head(e, i()), j()), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::load_variadic: /* --------------------------------------
        *
        *  s  e (%load-variadic <variadic> . c) => (x . s) e c
        *
        *  where <variadic> = (i . j)
        *
        *        x = (list-tail (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(tail(head(e, i()), j()), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::load_constant: /* --------------------------------------
        *
        *  s e (%load-constant <object> . c) => (x . s) e c
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(cadr(c), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::load_closure: /* ---------------------------------------
        *
        *  s e (%load-closure c' . c) => (<closure> . s) e c
        *
        *  where <closure> = (c' . e)
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(make<closure, segregated_storage_allocator<void>>(cadr(c), e), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::current: /* --------------------------------------------
        *
        *  s e (%current i . c) => (a[i] . s) e c
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<small_integer>());
        s.reset<bx, b1>(cons(a[exact_integer_cast<std::size_t>(cadr(c))], s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::select: /* ---------------------------------------------
        *
        *  (<boolean> . s) e (%select c1 c2) => s e c'
        *
        *  where c' = (if <boolean> c1 c2)
        *
        * ------------------------------------------------------------------- */
        assert(cdddr(c).template is<null>());
        c.reset<b1, b1>(car(s) != f ? cadr(c) : caddr(c));
        s.reset<b1, bx>(cdr(s));
        goto fetch;

      case instruction::call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> . xs) e (%call) => () (xs . e') c'
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          assert(cdr(c).template is<null>());
          c.reset<b1, b1>(car(callee));
          e.reset<bx, b1>(cons(cdr(s), cdr(callee)));
          s.reset<b1>();
          goto fetch;
        }
        else if (callee.is<procedure>()) /* ------------------------------------
        *
        *  (<procedure> k . xs) e (%call) => k(x)
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          assert(cdr(c).template is<null>());

          if (let const& k = cadr(s); k.is<closure>())
          {
            c.reset<b1, b1>(car(k));
            e.reset<bx, b1>(cons(list(callee.as<procedure>().call(cddr(s))), cdr(k)));
            s.reset<b1>();
            goto fetch;
          }
          else if (k.is<continuation>())
          {
            return callee.as<procedure>().call(cddr(s));
          }
          else
          {
            assert(false);
            s.reset<b1, bx>(list(k, callee.as<procedure>().call(cddr(s))));
            goto fetch;
          }
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> x . xs) e (%call) => x
        *
        * ------------------------------------------------------------------- */
        {
          assert(cdr(s).template is<pair>());
          assert(cdr(c).template is<null>());
          return cadr(s);
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
        }

      case instruction::drop: /* -----------------------------------------------
        *
        *  (x . s) e (%drop . c) => s e c
        *
        * ------------------------------------------------------------------- */
        s.reset<b1, bx>(cdr(s));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::store_absolute: /* -------------------------------------
        *
        *  (x . s) e (%store-absolute <absolute> . c) => (x . s) e c
        *
        *  where <absolute> = (<symbol> . <object>)
        *
        *        (set-cdr! <absolute> x)
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<absolute>());
        cdadr(c) = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::store_relative: /* -------------------------------------
        *
        *  (x . s) e (%store-relative <relative> . c) => (x . s) e c
        *
        * ------------------------------------------------------------------- */
        head(head(e, i()), j()) = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::store_variadic: /* -------------------------------------
        *
        *  (x . s) e (%store-variadic <variadic> . c) => (x . s) e c
        *
        * ------------------------------------------------------------------- */
        tail(head(e, i()), j()) = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::install: /* --------------------------------------------
        *
        *  (x . s) e (%install i . c) => (x . s) e c
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<small_integer>());
        a[exact_integer_cast<std::size_t>(cadr(c))] = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::list_values: /* ----------------------------------------
        *
        *  (xs) e (%list-values . c) => xs e c
        *
        * ------------------------------------------------------------------- */
        s.reset<b1, bx>(car(s));
        c.reset<b1, b1>(cdr(c));
        goto fetch;
#else
    try
    {
      let * c0 = c.as<vector>().objects.data();
      let * pc = c0;

    fetch:
      switch ((pc++)->as<instruction>())
      {
      case instruction::load_absolute:
        assert(pc->is_also<absolute>());
        s.reset<bx, b1>(cons(cdr(*pc), s));
        ++pc;
        goto fetch;

      case instruction::load_relative:
        assert(pc->is_also<relative>());
        s.reset<bx, b1>(cons(head(head(e, static_cast<std::size_t>(car(*pc).as<small_integer>())), static_cast<std::size_t>(cdr(*pc).as<small_integer>())), s));
        ++pc;
        goto fetch;

      case instruction::load_variadic:
        assert(pc->is_also<variadic>());
        s.reset<bx, b1>(cons(tail(head(e, static_cast<std::size_t>(car(*pc).as<small_integer>())), static_cast<std::size_t>(cdr(*pc).as<small_integer>())), s));
        ++pc;
        goto fetch;

      case instruction::load_constant:
        s.reset<bx, b1>(cons(*pc, s));
        ++pc;
        goto fetch;

      case instruction::load_closure:
        assert(pc->is<small_integer>());
        s.reset<bx, b1>(cons(make<closure, segregated_storage_allocator<void>>(cons(c, make<small_integer>(static_cast<small_integer>(pc - c0) + pc->as<small_integer>())), e), s));
        ++pc;
        goto fetch;

      case instruction::current:
        s.reset<bx, b1>(cons(a[static_cast<std::size_t>(pc->as<small_integer>())], s));
        ++pc;
        goto fetch;

      case instruction::select:
        assert(pc[0].is<small_integer>());
        assert(pc[1].is<small_integer>());
        pc += (car(s) != f ? pc[0] : pc[1]).as<small_integer>();
        s.reset<b1, bx>(cdr(s));
        goto fetch;

      case instruction::call:
        if (let const& f = car(s); f.is<closure>())
        {
          e.reset<bx, b1>(cons(cdr(s), cdr(f)));
          s.reset<bx>();

          if (c != caar(f))
          {
            c.reset<b1, b1>(caar(f));
            c0 = c.as<vector>().objects.data();
          }

          pc = c0 + static_cast<std::size_t>(cdar(f).as<small_integer>());
          goto fetch;
        }
        else if (f.is<procedure>())
        {
          if (let const& k = cadr(s); k.is<closure>())
          {
            e.reset<bx, b1>(cons(list(f.as<procedure>().call(cddr(s))), cdr(k)));
            s.reset<bx>();

            if (c != caar(k))
            {
              c.reset<b1, b1>(caar(k));
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
        s.reset<b1, bx>(cdr(s));
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
        s.reset<b1, bx>(car(s));
        goto fetch;
#endif
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
