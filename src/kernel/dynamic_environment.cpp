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

namespace meevax::inline kernel
{
  auto dynamic_environment::apply(object const& f, object const& xs) -> object
  {
    return execute(cons(f, make<continuation>(nullptr, list(list(make<instruction>(instruction::secd_stop)))), xs),
                   nullptr,
                   list(make<instruction>(instruction::secd_tail_call)),
                   nullptr);
  }

  auto dynamic_environment::execute(object const& c) -> object
  {
    return execute(nullptr, nullptr, c, nullptr);
  }

  auto dynamic_environment::execute(object s, object e, object c, object d) -> object
  {
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
      case instruction::secd_load_absolute: /* ---------------------------------
        *
        *  s e (%load-absolute <absolute> . c) d => (x . s) e c d
        *
        *  where <absolute> = (<symbol> . x)
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is_also<absolute>());
        s.reset<bx, b1>(cons(cdadr(c), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_load_relative: /* ---------------------------------
        *
        *  s  e (%load-relative <relative> . c) d => (x . s) e c d
        *
        *  where <relative> = (i . j)
        *
        *        x = (list-ref (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(head(head(e, i()), j()), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_load_variadic: /* ---------------------------------
        *
        *  s  e (%load-variadic <variadic> . c) d => (x . s) e c d
        *
        *  where <variadic> = (i . j)
        *
        *        x = (list-tail (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(tail(head(e, i()), j()), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_load_constant: /* ---------------------------------
        *
        *  s e (%load-constant <object> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(cadr(c), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_load_null: /* -------------------------------------
        *
        *  s e (%load-null . c) d => (() . s) e c d
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        s.reset<bx, b1>(cons(nullptr, s));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_load_closure: /* ----------------------------------
        *
        *  s e (%load-closure c' . c) d => (<closure> . s) e c d
        *
        *  where <closure> = (c' . e)
        *
        * ------------------------------------------------------------------- */
        s.reset<bx, b1>(cons(make<closure, segregated_storage_allocator<void>>(cadr(c), e), s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_load_continuation: /* -----------------------------
        *
        *  () e (%load-continuation c' . c) d => (<continuation>) e c d
        *
        *  where <continuation> = (e c' . d)
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        assert(s.is<null>());
        s.reset<b0, b1>(list(make<continuation>(e, cons(cadr(c), d))));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_current: /* ---------------------------------------
        *
        *  s e (%current i . c) => (a[i] . s) e c d
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<small_integer>());
        s.reset<bx, b1>(cons(a[exact_integer_cast<std::size_t>(cadr(c))], s));
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_select: /* ----------------------------------------
        *
        *  (<boolean> . s) e (%select c1 c2) d => s e c' d
        *
        *  where c' = (if <boolean> c1 c2)
        *
        * ------------------------------------------------------------------- */
        assert(cdddr(c).template is<null>());
        c.reset<b1, b1>(car(s) != f ? cadr(c) : caddr(c));
        s.reset<b1, bx>(cdr(s));
        goto fetch;

      case instruction::secd_call:
        assert(false); // No longer emitted.

        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> . xs) e (%call . c) d => () (xs . e') c' (e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<pair>());
          d.reset<bx, b1>(cons(e, cdr(c), d));
          c.reset<b1, b1>(car(callee));
          e.reset<bx, b1>(cons(cdr(s), cdr(callee)));
          s.reset<b1>();
          goto fetch;
        }
        else if (callee.is<procedure>()) /* ------------------------------------
        *
        *  (<procedure> . xs) e (%call . c) d => (x) e c d
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<pair>());
          s.reset<b1, b1>(list(callee.as<procedure>().call(cdr(s))));
          c.reset<b1, b1>(cdr(c));
          goto fetch;
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> . xs) e (%call . c) d => xs e' c' d'
        *
        *  where <continuation> = (e' c' . d')
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<pair>());
          e.reset<bx, bx>(car(callee));
          c.reset<b1, b1>(cadr(callee));
          d.reset<bx, bx>(cddr(callee));
          s.reset<b1, bx>(cdr(s));
          goto fetch;
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
        }

      case instruction::secd_tail_call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> . xs) e (%tail-call) d => () (xs . e') c' d
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<null>());
          c.reset<b1, b1>(car(callee));
          e.reset<bx, b1>(cons(cdr(s), cdr(callee)));
          s.reset<b1>();
          goto fetch;
        }
        else if (callee.is<procedure>()) /* ------------------------------------
        *
        *  (<procedure> k . xs) e (%tail-call) d => (k x) e (%tail-call) d
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<null>());
          s.reset<b1, bx>(list(cadr(s), callee.as<procedure>().call(cddr(s))));
          assert(not car(s).template is<procedure>());
          goto fetch;
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> . xs) e (%tail-call) d => xs e' c' d'
        *
        *  where <continuation> = (e' c' . d')
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<null>());
          d.reset<bx, bx>(cddr(callee));
          c.reset<b1, b1>(cadr(callee));
          e.reset<bx, bx>(car(callee));
          s.reset<b1, bx>(cdr(s));
          goto fetch;
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
        }

      case instruction::secd_dummy: /* -----------------------------------------
        *
        *  s e (%dummy . c) d => s (<null> . e) c d
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        e.reset<bx, b1>(cons(nullptr, e));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_letrec: /* ----------------------------------------
        *
        *  (<closure> . xs) (<null> . e) (%letrec . c) d => () (set-car! e' xs) c' (e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        cadar(s) = cdr(s);
        d.reset<bx, b1>(cons(cdr(e), cdr(c), d));
        c.reset<b1, b1>(caar(s));
        e.reset<b1, b1>(cdar(s));
        s.reset<b1>();
        goto fetch;

      case instruction::secd_tail_letrec: /* -----------------------------------
        *
        *  (<closure> . xs) (<null> . e) (%tail-letrec) d => () (set-car! e' xs) c' d
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        assert(cdr(c).template is<null>());
        cadar(s) = cdr(s);
        c.reset<b1, bx>(caar(s));
        e.reset<b1, b1>(cdar(s));
        s.reset<b1>();
        goto fetch;

      case instruction::secd_return: /* ----------------------------------------
        *
        *  s e (%return) (e' c' . d) => s e' c' d
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        assert(cdr(c).template is<null>());
        e.reset<bx, bx>(car(d));
        c.reset<b1, b1>(cadr(d));
        d.reset<b1, bx>(cddr(d));
        goto fetch;

      case instruction::secd_cons: /* ------------------------------------------
        *
        *  (x y . s) e (%cons . c) d => ((x . y) . s) e c d
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        car(s).reset<bx, b1>(cons(car(s), cadr(s)));
        cdr(s).reset<b1, bx>(cddr(s));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_cons_values: /* -----------------------------------
        *
        *  (x) e (%cons-values . c) (s' . d) => (x . s') e c d
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        s.reset<b1, b1>(cons(car(s), car(d)));
        d.reset<b1, bx>(cdr(d));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_drop: /* ------------------------------------------
        *
        *  (x . s) e (%drop . c) d => s e c d
        *
        * ------------------------------------------------------------------- */
        s.reset<b1, bx>(cdr(s));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_store_absolute: /* --------------------------------
        *
        *  (x . s) e (%store-absolute <absolute> . c) d => (x . s) e c d
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

      case instruction::secd_store_relative: /* --------------------------------
        *
        *  (x . s) e (%store-relative <relative> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        head(head(e, i()), j()) = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_store_variadic: /* --------------------------------
        *
        *  (x . s) e (%store-variadic <variadic> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        tail(head(e, i()), j()) = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_install: /* ---------------------------------------
        *
        *  (x . s) e (%install i . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<small_integer>());
        a[exact_integer_cast<std::size_t>(cadr(c))] = car(s);
        c.reset<b1, b1>(cddr(c));
        goto fetch;

      case instruction::secd_drop_values: /* -----------------------------------
        *
        *  s e (%drop-values . c) d => () e c d
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        s.reset<bx>();
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_save_values: /* -----------------------------------
        *
        *  s e (%save-values . c) d => () e c (s . d)
        *
        * ------------------------------------------------------------------- */
        assert(false); // No longer emitted.
        d.reset<bx, b1>(cons(s, d));
        s.reset<bx>();
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      case instruction::secd_list_values: /* -----------------------------------
        *
        *  (xs) e (%list-values . c) d => xs e c d
        *
        * ------------------------------------------------------------------- */
        s.reset<b1, bx>(car(s));
        c.reset<b1, b1>(cdr(c));
        goto fetch;

      default: // ERROR
        assert(false);
        [[fallthrough]];

      case instruction::secd_stop: /* ------------------------------------------
        *
        *  (x . xs) () (%stop) () => (x . xs) () () ()
        *
        * ------------------------------------------------------------------- */
        assert(cdr(s).template is<null>());
        assert(e.is<null>());
        assert(cdr(c).template is<null>());
        assert(d.is<null>());
        return car(s);
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
