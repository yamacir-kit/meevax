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

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/dynamic_environment.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/procedure.hpp>

namespace meevax::inline kernel
{
  auto dynamic_environment::execute(object const& control) -> object
  {
    assert(s.is<null>());

    c = control;

    assert(last(c).is<instruction>());
    assert(last(c).as<instruction>() == instruction::stop);

    return run();
  }

  auto dynamic_environment::run() -> object
  {
    assert(last(c).template is<instruction>());
    assert(last(c).template as<instruction>() == instruction::stop);

    let const control = c;

    try
    {
    fetch:
      assert(c);

      switch (car(c).template as<instruction>())
      {
      case instruction::load_absolute: /* --------------------------------------
        *
        *  s e (%load-absolute <absolute> . c) d => (x . s) e c d
        *
        *  where <absolute> = (<symbol> . x)
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is_also<absolute>());

        if (let const& x = cdadr(c); x == undefined)
        {
          throw error(make<string>("undefined variable"), caadr(c));
        }
        else
        {
          s = cons(x, s);
          c = cddr(c);
          goto fetch;
        }

      case instruction::load_relative: /* --------------------------------------
        *
        *  s  e (%load-relative <relative> . c) d => (x . s) e c d
        *
        *  where <relative> = (i . j)
        *
        *        x = (list-ref (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        {
          let const& operand = cadr(c);

          assert(operand.is<relative>());

          assert(car(operand).is<std::int32_t>());
          assert(cdr(operand).is<std::int32_t>());

          auto i = car(operand).as<std::int32_t>();
          auto j = cdr(operand).as<std::int32_t>();

          assert(i < length(e));

          s = cons(head(head(e, i), j), s);
          c = cddr(c);
        }
        goto fetch;

      case instruction::load_variadic: /* --------------------------------------
        *
        *  s  e (%load-variadic <variadic> . c) d => (x . s) e c d
        *
        *  where <variadic> = (i . j)
        *
        *        x = (list-tail (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        {
          let const& operand = cadr(c);

          assert(operand.is<variadic>());

          assert(car(operand).is<std::int32_t>());
          assert(cdr(operand).is<std::int32_t>());

          auto i = car(operand).as<std::int32_t>();
          auto j = cdr(operand).as<std::int32_t>();

          assert(i < length(e));

          s = cons(tail(head(e, i), j), s);
          c = cddr(c);
        }
        goto fetch;

      case instruction::load_constant: /* --------------------------------------
        *
        *  s e (%load-constant <object> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        s = cons(cadr(c), s);
        c = cddr(c);
        goto fetch;

      case instruction::load_closure: /* ---------------------------------------
        *
        *  s e (%load-closure c' . c) d => (<closure> . s) e c d
        *
        *  where <closure> = (c' . e)
        *
        * ------------------------------------------------------------------- */
        s = cons(make<closure, allocator<void>>(cadr(c), e), s);
        c = cddr(c);
        goto fetch;

      case instruction::load_continuation: /* ----------------------------------
        *
        *  s e (%load-continuation c' . c) d => ((<continuation>) . s) e c d
        *
        *  where <continuation> = (s e c' . d)
        *
        * ------------------------------------------------------------------- */
        s = cons(list(make<continuation, allocator<void>>(s, cons(e, cons(cadr(c), d)))), s);
        c = cddr(c);
        goto fetch;

      case instruction::current: /* --------------------------------------------
        *
        *  s e (%current i . c) => (a[i] . s) e c d
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<std::int32_t>());
        s = cons(a[size_cast(cadr(c))], s);
        c = cddr(c);
        goto fetch;

      case instruction::select: /* ---------------------------------------------
        *
        *  (<boolean> . s) e (%select c1 c2 . c) d => s e c' (c . d)
        *
        *  where c' = (if <boolean> c1 c2)
        *
        * ------------------------------------------------------------------- */
        d = cons(cdddr(c), d);
        c = car(s) != f ? cadr(c) : caddr(c);
        s = cdr(s);
        goto fetch;

      case instruction::tail_select: /* ----------------------------------------
        *
        *  (<boolean> . s) e (%tail-select c1 c2) d => s e c' d
        *
        *  where c' = (if <boolean> c1 c2)
        *
        * ------------------------------------------------------------------- */
        assert(cdddr(c).template is<null>());
        c = car(s) != f ? cadr(c) : caddr(c);
        s = cdr(s);
        goto fetch;

      case instruction::join: /* -----------------------------------------------
        *
        *  s e (%join) (c . d) => s e c d
        *
        * ------------------------------------------------------------------- */
        assert(cdr(c).template is<null>());
        c = car(d);
        d = cdr(d);
        goto fetch;

      case instruction::call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> xs . s) e (%call . c) d => () (xs . e') c' (s e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<pair>());
          d = cons(cddr(s), e, cdr(c), d);
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = nullptr;
          goto fetch;
        }
        else if (callee.is_also<primitive>()) /* -------------------------------
        *
        *  (<primitive> xs . s) e (%call . c) d => (x . s) e c d
        *
        *  where x = primitive-procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(c, 1).template is<pair>());
          s = cons(callee.as<primitive>()(cadr(s)), cddr(s));
          c = cdr(c);
          goto fetch;
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> xs) e (%call . c) d => (xs . s') e' c' d'
        *
        *  where <continuation> = (s' e' c' . 'd)
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(s, 2).template is<null>());
          assert(tail(c, 1).template is<pair>());
          s = cons(caadr(s), car(callee));
          e = cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
          goto fetch;
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
        }

      case instruction::tail_call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> xs) e (%tail-call) d => () (xs . e') c' d
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(s, 2).template is<null>());
          assert(tail(c, 1).template is<null>());
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = nullptr;
          goto fetch;
        }
        else if (callee.is_also<primitive>()) /* -------------------------------
        *
        *  (<primitive> xs) e (%tail-call) (s' e' c' . d) => (x . s') e' c' d
        *
        *  where x = primitive-procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(s, 2).template is<null>());
          assert(tail(c, 1).template is<null>());
          s = cons(callee.as<primitive>()(cadr(s)), car(d));
          e = cadr(d);
          c = caddr(d);
          d = cdddr(d);
          goto fetch;
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> xs) e (%tail-call) d => (xs . s') e' c' d'
        *
        *  where <continuation> = (s' e' c' . 'd)
        *
        * ------------------------------------------------------------------- */
        {
          assert(tail(s, 2).template is<null>());
          assert(tail(c, 1).template is<null>());
          s = cons(caadr(s), car(callee));
          e = cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
          goto fetch;
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
        }

      case instruction::dummy: /* ----------------------------------------------
        *
        *  s e (%dummy . c) d => s (<null> . e) c d
        *
        * ------------------------------------------------------------------- */
        e = cons(nullptr, e);
        c = cdr(c);
        goto fetch;

      case instruction::letrec: /* ---------------------------------------------
        *
        *  (<closure> xs . s) (<null> . e) (%letrec . c) d => () (set-car! e' xs) c' (s e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        cadar(s) = cadr(s);
        d = cons(cddr(s), cdr(e), cdr(c), d);
        c = caar(s);
        e = cdar(s);
        s = nullptr;
        goto fetch;

      case instruction::tail_letrec: /* ----------------------------------------
        *
        *  (<closure> xs . s) (<null> . e) (%tail-letrec) d => () (set-car! e' xs) c' d
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        assert(cdr(c).template is<null>());
        cadar(s) = cadr(s);
        c = caar(s);
        e = cdar(s);
        s = nullptr;
        goto fetch;

      case instruction::return_: /* --------------------------------------------
        *
        *  (x)  e (%return) (s' e' c' . d) => (x . s') e' c' d
        *
        * ------------------------------------------------------------------- */
        assert(cdr(s).template is<null>());
        assert(cdr(c).template is<null>());

        s = cons(car(s), car(d));
        e = cadr(d);
        c = caddr(d);
        d = cdddr(d);
        goto fetch;

      case instruction::cons: /* -----------------------------------------------
        *
        *  (x y . s) e (%cons . c) d => ((x . y) . s) e c d
        *
        * ------------------------------------------------------------------- */
        car(s) = cons(car(s), cadr(s));
        cdr(s) = cddr(s);
        c = cdr(c);
        goto fetch;

      case instruction::drop: /* -----------------------------------------------
        *
        *  (x . s) e (%drop . c) d => s e c d
        *
        * ------------------------------------------------------------------- */
        s = cdr(s);
        c = cdr(c);
        goto fetch;

      case instruction::store_absolute: /* -------------------------------------
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
        c = cddr(c);
        goto fetch;

      case instruction::store_relative: /* -------------------------------------
        *
        *  (x . s) e (%store-relative <relative> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        {
          let const& operand = cadr(c);

          assert(operand.is<relative>());

          assert(car(operand).is<std::int32_t>());
          assert(cdr(operand).is<std::int32_t>());

          auto i = car(operand).as<std::int32_t>();
          auto j = cdr(operand).as<std::int32_t>();

          assert(i < length(e));

          head(head(e, i), j) = car(s);

          c = cddr(c);
        }
        goto fetch;

      case instruction::store_variadic: /* -------------------------------------
        *
        *  (x . s) e (%store-variadic <variadic> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        {
          let const& operand = cadr(c);

          assert(operand.is<variadic>());

          assert(car(operand).is<std::int32_t>());
          assert(cdr(operand).is<std::int32_t>());

          auto i = car(operand).as<std::int32_t>();
          auto j = cdr(operand).as<std::int32_t>();

          assert(i < length(e));

          tail(head(e, i), j) = car(s);

          c = cddr(c);
        }
        goto fetch;

      case instruction::install: /* --------------------------------------------
        *
        *  (x . s) e (%install i . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        assert(cadr(c).template is<std::int32_t>());
        a[size_cast(cadr(c))] = car(s);
        c = cddr(c);
        goto fetch;

      default: // ERROR
      case instruction::stop: /* -----------------------------------------------
        *
        *  (x) e (%stop) d => () e () d
        *
        * ------------------------------------------------------------------- */
        return [this]()
        {
          assert(cdr(s).template is<null>());
          assert(cdr(c).template is<null>());

          let const x = car(s);

          s = cdr(s);
          c = cdr(c);

          return x;
        }();
      }
    }
    catch (object const& thrown) // by the primitive procedure `throw`.
    {
      if (thrown.is_also<error>())
      {
        thrown.as<error>().raise();
        return unspecified;
      }
      else
      {
        error::contexts.emplace_back(error::in::running, cons(control, c));
        throw error(make<string>("uncaught exception"), thrown);
      }
    }
    catch (error & thrown) // by any primitive procedure other than `throw`.
    {
      if (exception_handler)
      {
        error::contexts.emplace_back(error::in::running, cons(control, c));
        return apply(exception_handler, thrown.make());
      }
      else // In most cases, this clause will never be called.
      {
        thrown.raise();
        return unspecified;
      }
    }
    catch (std::exception const& exception) // by the system.
    {
      if (auto thrown = error(make<string>(exception.what())); exception_handler)
      {
        error::contexts.emplace_back(error::in::running, cons(control, c));
        return apply(exception_handler, thrown.make());
      }
      else // In most cases, this clause will never be called.
      {
        throw thrown;
      }
    }
  }
} // namespace meevax::kernel
