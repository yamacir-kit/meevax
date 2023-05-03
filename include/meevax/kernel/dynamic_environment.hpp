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

#ifndef INCLUDED_MEEVAX_KERNEL_DYNAMIC_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_KERNEL_DYNAMIC_ENVIRONMENT_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct dynamic_environment
  {
    /*
       The SECD machine, which in its original form was invented by Landin,
       derives its name from the designation of its four pricipal registers:

       s   the stack          Used to hold intermediate results when computing
                              the values of expressions.

       e   the environment    Used to hold the values bound to variables during
                              evaluation.

       c   the control list   Used to hold the machine-language program being
                              executed.

       d   the dump           Used as a stack to save values of other registers
                              on calling a new function.
    */
    let s, e, c, d;

    /*
       Auxiliary register.

       a[0] is used for current-dynamic-extents (dynamic-wind).
       a[1] is used for current-dynamic-bindings (make-parameter and parameterize).
       a[2] is used for current-exception-handler (with-exception-handler, raise and raise-continuable).
    */
    std::array<let, 3> a;

    /*
       raise is a one-argument procedure for propagating C++ exceptions thrown
       in the Meevax kernel to the exception handler of the language running on
       the Meevax kernel.

       raise is set to null by default. In this default state, that is, if raise
       is null, C++ exceptions thrown in the kernel are rethrown to the outer
       environment.

       Although raise can be set to any one-argument procedure by procedure
       `kernel-exception-handler-set!`, it is basically assumed to be set to
       R7RS Scheme's standard procedure `raise`.
    */
    let static inline raise = unit;

    template <typename... Ts>
    auto apply(object const& f, Ts&&... xs) -> object
    {
      s = list(f, list(std::forward<decltype(xs)>(xs)...));
      c = list(make(instruction::call),
               make(instruction::stop));

      return run();
    }

    auto execute(object const& control) -> object
    {
      assert(s.is<null>());

      c = control;

      assert(last(c).is<instruction>());
      assert(last(c).as<instruction>() == instruction::stop);

      return run();
    }

    auto reraise(object const& x) -> object
    {
      return raise.is<null>() ? throw x : apply(raise, x);
    }

    auto run() -> object
    {
      assert(last(c).template is<instruction>());
      assert(last(c).template as<instruction>() == instruction::stop);

      try
      {
      fetch:
        assert(c);

        if constexpr (false)
        {
          std::cerr << faint("; s = ") << s << "\n"
                    << faint("; e = ") << e << "\n"
                    << faint("; c = ") << c << "\n"
                    << faint("; d = ") << d << "\n" << std::endl;
        }

        if constexpr (profiler::count_instruction_fetch)
        {
          current_profiler().instruction_fetchs[car(c).template as<instruction>()]++;
        }

        switch (car(c).template as<instruction>())
        {
        case instruction::load_absolute: /* ------------------------------------
          *
          *  s e (%load-absolute <absolute> . c) d => (x . s) e c d
          *
          *  where <absolute> = (<symbol> . x)
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<absolute>());

          if (let const& value = cadr(c).template as<absolute>().load(); value == undefined)
          {
            throw error(make<string>("undefined variable"), cadr(c).template as<absolute>().symbol());
          }
          else
          {
            s = cons(cadr(c).template as<absolute>().load(), s);
            c = cddr(c);
            goto fetch;
          }

        case instruction::load_relative: /* ------------------------------------
          *
          *  s  e (%load-relative <relative> . c) d => (x . s) e c d
          *
          *  where <relative> = (<symbol> i . j)
          *
          *        x = (list-ref (list-ref e i) j)
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<relative>());
          s = cons(cadr(c).template as<relative>().load(e), s);
          c = cddr(c);
          goto fetch;

        case instruction::load_variadic: /* ------------------------------------
          *
          *  s  e (%load-variadic <variadic> . c) d => (x . s) e c d
          *
          *  where <variadic> = (<symbol> i . j)
          *
          *        x = (list-tail (list-ref e i) j)
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<variadic>());
          s = cons(cadr(c).template as<variadic>().load(e), s);
          c = cddr(c);
          goto fetch;

        case instruction::load_constant: /* ------------------------------------
          *
          *  s e (%load-constant <object> . c) d => (x . s) e c d
          *
          * ----------------------------------------------------------------- */
          s = cons(cadr(c), s);
          c = cddr(c);
          goto fetch;

        case instruction::load_closure: /* -------------------------------------
          *
          *  s e (%load-closure c' . c) d => (<closure> . s) e c d
          *
          *  where <closure> = (c' . e)
          *
          * ----------------------------------------------------------------- */
          s = cons(make<closure>(cadr(c), e), s);
          c = cddr(c);
          goto fetch;

        case instruction::load_continuation: /* --------------------------------
          *
          *  s e (%load-continuation c1 . c2) d => ((<continuation>) . s) e c2 d
          *
          *  where <continuation> = (s e c1 . d)
          *
          * ----------------------------------------------------------------- */
          s = cons(list(make<continuation>(s, e, cadr(c), d)), s);
          c = cddr(c);
          goto fetch;

        case instruction::current: /* ------------------------------------------
          *
          *  s e (%current i . c) => (a[i] . s) e c d
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<exact_integer>());
          s = cons(a[cadr(c).template as<exact_integer>()], s);
          c = cddr(c);
          goto fetch;

        case instruction::select: /* -------------------------------------------
          *
          *  (<boolean> . s) e (%select c1 c2 . c) d => s e c' (c . d)
          *
          *  where c' = (if <boolean> c1 c2)
          *
          * ----------------------------------------------------------------- */
          d = cons(cdddr(c), d);
          [[fallthrough]];

        case instruction::tail_select: /* --------------------------------------
          *
          *  (<boolean> . s) e (%select c1 c2 . c) d => s e c' d
          *
          *  where c' = (if <boolean> c1 c2)
          *
          * ----------------------------------------------------------------- */
          c = is_truthy(car(s)) ? cadr(c) : caddr(c);
          s = cdr(s);
          goto fetch;

        case instruction::join: /* ---------------------------------------------
          *
          *  s e (%join) (c . d) => s e c d
          *
          * ----------------------------------------------------------------- */
          assert(cdr(c).template is<null>());
          c = car(d);
          d = cdr(d);
          goto fetch;

        case instruction::call:
          if (let const& callee = car(s); callee.is<closure>()) /* -------------
          *
          *  (<closure> xs . s) e (%call . c) d => () (xs . e') c' (s e c . d)
          *
          *  where <closure> = (c' . e')
          *
          * ----------------------------------------------------------------- */
          {
            assert(tail(c, 1).template is<pair>());
            d = cons(cddr(s), e, cdr(c), d);
            c =               callee.as<closure>().c();
            e = cons(cadr(s), callee.as<closure>().e());
            s = unit;
            goto fetch;
          }
          else if (callee.is_also<procedure>()) /* -----------------------------
          *
          *  (<procedure> xs . s) e (%call . c) d => (x . s) e c d
          *
          *  where x = procedure(xs)
          *
          * ----------------------------------------------------------------- */
          {
            assert(tail(c, 1).template is<pair>());
            s = cons(callee.as<procedure>().call(cadr(s)), cddr(s));
            c = cdr(c);
            goto fetch;
          }
          else if (callee.is<continuation>()) /* -------------------------------
          *
          *  (<continuation> xs) e (%call . c) d => (xs . s') e' c' d'
          *
          *  where <continuation> = (s' e' c' . 'd)
          *
          * ----------------------------------------------------------------- */
          {
            assert(tail(s, 2).template is<null>());
            assert(tail(c, 1).template is<pair>());
            s = cons(caadr(s), callee.as<continuation>().s());
            e =                callee.as<continuation>().e();
            c =                callee.as<continuation>().c();
            d =                callee.as<continuation>().d();
            goto fetch;
          }
          else
          {
            throw error(make<string>("not applicable"), callee);
          }

        case instruction::tail_call:
          if (let const& callee = car(s); callee.is<closure>()) /* -------------
          *
          *  (<closure> xs) e (%tail-call) d => () (xs . e') c' d
          *
          *  where <closure> = (c' . e')
          *
          * ----------------------------------------------------------------- */
          {
            assert(tail(s, 2).template is<null>());
            assert(tail(c, 1).template is<null>());
            c =               callee.as<closure>().c();
            e = cons(cadr(s), callee.as<closure>().e());
            s = unit;
            goto fetch;
          }
          else if (callee.is_also<procedure>()) /* -----------------------------
          *
          *  (<procedure> xs) e (%tail-call) (s' e' c' . d) => (x . s') e' c' d
          *
          *  where x = procedure(xs)
          *
          * ----------------------------------------------------------------- */
          {
            assert(tail(s, 2).template is<null>());
            assert(tail(c, 1).template is<null>());
            s = cons(callee.as<procedure>().call(cadr(s)), car(d));
            e = cadr(d);
            c = caddr(d);
            d = cdddr(d);
            goto fetch;
          }
          else if (callee.is<continuation>()) /* -------------------------------
          *
          *  (<continuation> xs) e (%tail-call) d => (xs . s') e' c' d'
          *
          *  where <continuation> = (s' e' c' . 'd)
          *
          * ----------------------------------------------------------------- */
          {
            assert(tail(s, 2).template is<null>());
            assert(tail(c, 1).template is<null>());
            s = cons(caadr(s), callee.as<continuation>().s());
            e =                callee.as<continuation>().e();
            c =                callee.as<continuation>().c();
            d =                callee.as<continuation>().d();
            goto fetch;
          }
          else
          {
            throw error(make<string>("not applicable"), callee);
          }

        case instruction::dummy: /* --------------------------------------------
          *
          *  s e (%dummy . c) d => s (<null> . e) c d
          *
          * ----------------------------------------------------------------- */
          e = cons(unit, e);
          c = cdr(c);
          goto fetch;

        case instruction::tail_letrec: /* --------------------------------------
          *
          *  (<closure> xs . s) (<unit> . e) (%letrec . c) d => () (set-car! e' xs) c' d
          *
          *  where <closure> = (c' . e')
          *
          * ----------------------------------------------------------------- */
          cadar(s) = cadr(s);
          c = caar(s);
          e = cdar(s);
          s = unit;
          goto fetch;

        case instruction::letrec: /* -------------------------------------------
          *
          *  (<closure> xs . s) (<unit> . e) (%letrec . c) d => () (set-car! e' xs) c' (s e c . d)
          *
          *  where <closure> = (c' . e')
          *
          * ----------------------------------------------------------------- */
          cadar(s) = cadr(s);
          d = cons(cddr(s), cdr(e), cdr(c), d);
          c = caar(s);
          e = cdar(s);
          s = unit;
          goto fetch;

        case instruction::return_: /* ------------------------------------------
          *
          *  (x)  e (%return) (s' e' c' . d) => (x . s') e' c' d
          *
          * ----------------------------------------------------------------- */
          assert(cdr(s).template is<null>());
          assert(cdr(c).template is<null>());

          s = cons(car(s), car(d));
          e = cadr(d);
          c = caddr(d);
          d = cdddr(d);
          goto fetch;

        case instruction::cons: /* ---------------------------------------------
          *
          *  (x y . s) e (%cons . c) d => ((x . y) . s) e c d
          *
          * ----------------------------------------------------------------- */
          s = cons(cons(car(s), cadr(s)), cddr(s));
          c = cdr(c);
          goto fetch;

        case instruction::drop: /* ---------------------------------------------
          *
          *  (x . s) e (%drop . c) d => s e c d
          *
          * ----------------------------------------------------------------- */
          s = cdr(s);
          c = cdr(c);
          goto fetch;

        case instruction::store_absolute: /* -----------------------------------
          *
          *  (x . s) e (%store-absolute <absolute> . c) d => (x . s) e c d
          *
          *  where <absolute> = (<symbol> . <object>)
          *
          *        (set-cdr! <absolute> x)
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<absolute>());
          cadr(c).template as<absolute>().store(car(s));
          c = cddr(c);
          goto fetch;

        case instruction::store_relative: /* -----------------------------------
          *
          *  (x . s) e (%store-relative <relative> . c) d => (x . s) e c d
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<relative>());
          cadr(c).template as<relative>().store(car(s), e);
          c = cddr(c);
          goto fetch;

        case instruction::store_variadic: /* -----------------------------------
          *
          *  (x . s) e (%store-variadic <variadic> . c) d => (x . s) e c d
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<variadic>());
          cadr(c).template as<variadic>().store(car(s), e);
          c = cddr(c);
          goto fetch;

        case instruction::install: /* ------------------------------------------
          *
          *  (x . s) e (%intall i . c) d => (x . s) e c d
          *
          * ----------------------------------------------------------------- */
          assert(cadr(c).template is<exact_integer>());
          a[cadr(c).template as<exact_integer>()] = car(s);
          c = cddr(c);
          goto fetch;

        default: // ERROR
        case instruction::stop: /* ---------------------------------------------
          *
          *  (x) e (%stop) d => () e () d
          *
          * ----------------------------------------------------------------- */
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
      catch (std::exception const& exception)
      {
        return reraise(make<error>(make<string>(exception.what())));
      }
      catch (error const& error)
      {
        return reraise(make(error));
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DYNAMIC_ENVIRONMENT_HPP