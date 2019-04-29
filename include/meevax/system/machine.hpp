#ifndef INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP

#include <meevax/system/boolean.hpp> // false_v
#include <meevax/system/closure.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/instruction.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/operator.hpp> // assoc
#include <meevax/system/procedure.hpp>
#include <meevax/system/symbol.hpp>
#include <meevax/system/syntax.hpp>

namespace meevax::system
{
  // Simple SECD machine.
  class machine
  {
    cursor s, // stack
           e, // local environment
           c, // code
           d; // dump

    cursor env; // global environment

    #define DEBUG_0() // std::cerr << "\x1B[?7l\t" << take(c, 1) << "\x1B[?7h" << std::endl
    #define DEBUG_1() // std::cerr << "\x1B[?7l\t" << take(c, 2) << "\x1B[?7h" << std::endl
    #define DEBUG_2() // std::cerr << "\x1B[?7l\t" << take(c, 3) << "\x1B[?7h" << std::endl

  public:
    machine(const cursor& env = unit)
      : env {env}
    {}

    // Direct virtual machine instruction invocation.
    template <typename... Ts>
    decltype(auto) define(const cursor& key, Ts&&... args)
    {
      return env = list(key, std::forward<Ts>(args)...) | env;
    }

    cursor compile(const cursor& exp,
                   const cursor& scope = unit,
                   const cursor& continuation = list(STOP))
    {
      if (not exp)
      {
        return cons(LDC, unit, continuation);
      }
      else if (not exp.is<pair>())
      {
        if (exp.is<symbol>()) // is variable
        {
          if (auto location {locate(exp, scope)}; location) // there is local-defined variable
          {
            // load variable value (bound to lambda parameter) at runtime
            return cons(LDX, location, continuation);
          }
          else
          {
            // load variable value from global-environment at runtime
            return cons(LDG, exp, continuation);
          }
        }
        else // is self-evaluation
        {
          return cons(LDC, exp, continuation);
        }
      }
      else // is (syntax-or-any-application . arguments)
      {
        if (auto buffer {assoc(car(exp), env)}; not there_is(car(exp), scope) && buffer && buffer.is<syntax>())
        {
          return buffer.as<syntax>()(exp, scope, continuation);
        }
        else // is (application . arguments)
        {
          return args(
                   cdr(exp),
                   scope,
                   compile(car(exp), scope, cons(APPLY, continuation))
                 );
        }
      }
    }

    auto execute(const cursor& exp) noexcept(false)
    {
      s = e = d = unit;

      c = exp;

    dispatch:
      switch (car(c).as<instruction>().code)
      {
      case instruction::secd::LDX:
        {
          // S E (LDX (i . j) . C) D => (value . S) E C D
          DEBUG_1();

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).as<number>()};

          // TODO Add LDV (load-variadic) instruction to remove this conditional.
          if (cursor lexical_scope {car(std::next(e, i))}; j < 0)
          {
            s = cons(std::next(lexical_scope, -++j), s);
          }
          else
          {
            s = cons(car(std::next(lexical_scope, j)), s);
          }

          c = cddr(c);
        }
        goto dispatch;

      case instruction::secd::LDC:
        // S E (LDC constant . C) D => (constant . S) E C D
        DEBUG_1();
        s = cons(cadr(c), s);
        c = cddr(c);
        goto dispatch;

      case instruction::secd::LDG:
        // S E (LDG symbol . C) D => (value . S) E C D
        DEBUG_1();

        if (const auto& var {assoc(cadr(c), env)}; var == undefined)
        {
          throw error {to_string(cadr(c), "\x0b[31m", " is unbound")};
        }
        else
        {
          s = cons(var, s);
        }

        c = cddr(c);
        goto dispatch;

      case instruction::secd::LDF:
        // S E (LDF code . C) => (closure . S) E C D
        DEBUG_1();
        s = cons(make<closure>(cadr(c), e), s);
        c = cddr(c);
        goto dispatch;

      case instruction::secd::SELECT:
        // (boolean . S) E (SELECT then else . C) D => S E then/else (C. D)
        DEBUG_2();
        d = cons(cdddr(c), d);
        c = (car(s) != false_v ? cadr(c) : caddr(c));
        s = cdr(s);
        goto dispatch;

      case instruction::secd::JOIN:
        // S E (JOIN . x) (C . D) => S E C D
        DEBUG_0();
        c = car(d);
        d = cdr(d);
        goto dispatch;

      case instruction::secd::CAR:
        DEBUG_0();
        car(s) = caar(s); // TODO check?
        c = cdr(c);
        goto dispatch;

      case instruction::secd::CDR:
        DEBUG_0();
        car(s) = cdar(s); // TODO check?
        c = cdr(c);
        goto dispatch;

      case instruction::secd::CONS:
        DEBUG_0();
        s = cons(cons(car(s), cadr(s)), cddr(s));
        c = cdr(c);
        goto dispatch;

      case instruction::secd::DEFINE:
        DEBUG_1();
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define (change to #<undefined>?)
        c = cddr(c);
        goto dispatch;

      case instruction::secd::STOP:
        // (result . S) E (STOP . C) D
        DEBUG_0();
        c = cdr(c);
        return car(s);

      case instruction::secd::APPLY:
        DEBUG_0();

        if (auto applicable {car(s)}; not applicable)
        {
          throw error {"unit is not applicable"};
        }
        else if (applicable.is<closure>()) // (closure args . S) E (APPLY . C) D
        {
          d = cons(cddr(s), e, cdr(c), d);
          c = car(applicable);
          e = cons(cadr(s), cdr(applicable));
          s = unit;
        }
        else if (applicable.is<procedure>()) // (procedure args . S) E (APPLY . C) D
        {
          // XXX This dynamic_cast is removable?
          s = cons(applicable.as<procedure>()(cadr(s)), cddr(s));
          c = cdr(c);
        }
        else
        {
          throw error {to_string(applicable, "\x1b[31m", " is not applicable")};
        }
        goto dispatch;

      case instruction::secd::RETURN:
        DEBUG_0();
        s = cons(car(s), car(d));
        e = cadr(d);
        c = caddr(d);
        d = cdddr(d);
        goto dispatch;

      default:
        throw error {to_string(car(c), " is not virtual machine instruction")};
      }

      throw error {to_string("unterminated execution")};
    }

    cursor begin(const cursor& exp,
                 const cursor& scope,
                 const cursor& continuation)
    {
      return compile(
                 car(exp),
                 scope,
                 cdr(exp) ? cons(POP, begin(cdr(exp), scope, continuation))
                          :                                  continuation
             );
    }

  protected: // Compilation Helpers
    cursor locate(const cursor& exp, const cursor& scope)
    {
      auto i {0}, j {0};

      for (auto x {scope}; x; ++x, ++i)
      {
        for (cursor y {car(x)}; y; ++y, ++j)
        {
          if (y.is<pair>() && car(y) == exp)
          {
            return cons(make<number>(i), make<number>(j));
          }

          if (!y.is<pair>() && y == exp)
          {
            return cons(make<number>(i), make<number>(-++j));
          }
        }
      }

      return unit;
    }

    bool there_is(const cursor& exp, const cursor& scope)
    {
      for (cursor frame : scope)
      {
        for (cursor each : frame)
        {
          if (each.is<pair>() && car(each) == exp)
          {
            return true;
          }

          if (!each.is<pair>() && each == exp)
          {
            return true;
          }
        }
      }

      return false;
    }

    cursor args(const cursor& exp,
                const cursor& scope,
                const cursor& continuation)
    {
      if (exp && exp.is<pair>())
      {
        return args(
                 cdr(exp),
                 scope,
                 compile(car(exp), scope, cons(CONS, continuation))
               );
      }
      else
      {
        return compile(exp, scope, continuation);
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP

