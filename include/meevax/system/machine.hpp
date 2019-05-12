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
  protected:
    cursor s, // stack
           e, // lexical environment
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
      return env.push(list(key, std::forward<Ts>(args)...));
    }

    objective compile(const objective& exp,
                      const objective& scope = unit,
                      const objective& continuation = list(STOP))
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
        if (const auto& buffer {assoc(car(exp), env)}; !buffer)
        {
          throw error {"unit is not appliciable"};
        }
        else if (buffer != unbound && buffer.is<native_syntax>() && not local_defined(car(exp), scope))
        {
          return buffer.as<native_syntax>()(exp, scope, continuation);
        }
        else if (buffer != unbound && buffer.is<syntax>() && not local_defined(car(exp), scope))
        {
          std::cerr << "[debug] expanding syntax: " << car(buffer) << std::endl;
          std::cerr << "        arguments: " << cdr(exp) << std::endl;

          machine expander {env};

          expander.s = unit;
          expander.e = list(cdr(exp));
          expander.d = cons(
                         unit,       // s
                         unit,       // e
                         list(STOP), // c
                         unit        // d
                       );

          auto expanded {expander.execute(car(buffer))};
          std::cerr << "        expanded: " << expanded << std::endl;

          return compile(expanded, scope, continuation);
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

    objective execute(const objective& exp) noexcept(false)
    {
      c = exp;

    dispatch:
      switch (c.top().as<instruction>().code)
      {
      case instruction::secd::LDX: // S E (LDX (i . j) . C) D => (value . S) E C D
        {
          DEBUG_1();

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).as<number>()};

          // TODO Add LDV (load-variadic) instruction to remove this conditional.
          if (cursor scope {car(std::next(e, i))}; j < 0)
          {
            s.push(std::next(scope, -++j));
          }
          else
          {
            s.push(car(std::next(scope, j)));
          }

          c.pop(2);
        }
        goto dispatch;

      case instruction::secd::LDC: // S E (LDC constant . C) D => (constant . S) E C D
        DEBUG_1();
        s.push(cadr(c));
        c.pop(2);
        goto dispatch;

      case instruction::secd::LDG: // S E (LDG symbol . C) D => (value . S) E C D
        DEBUG_1();

        if (const auto& var {assoc(cadr(c), env)}; var == unbound)
        {
          throw error {pseudo_display(cadr(c), "\x01b[31m", " is unbound")};
        }
        else
        {
          s.push(var);
        }

        c.pop(2);
        goto dispatch;

      case instruction::secd::LDS:
        DEBUG_1();
        s.push(make<syntax>(cadr(c), e));
        c.pop(2);
        goto dispatch;

      case instruction::secd::LDF: // S E (LDF code . C) => (closure . S) E C D
        DEBUG_1();
        s.push(make<closure>(cadr(c), e));
        c.pop(2);
        goto dispatch;

      case instruction::secd::SELECT: // (boolean . S) E (SELECT then else . C) D => S E then/else (C. D)
        DEBUG_2();
        d.push(cdddr(c));
        c = (car(s) != false_v ? cadr(c) : caddr(c));
        s.pop(1);
        goto dispatch;

      case instruction::secd::JOIN: // S E (JOIN . x) (C . D) => S E C D
        DEBUG_0();
        c = car(d);
        d.pop(1);
        goto dispatch;

      case instruction::secd::CAR:
        DEBUG_0();
        car(s) = caar(s); // TODO check?
        c.pop(1);
        goto dispatch;

      case instruction::secd::CDR:
        DEBUG_0();
        car(s) = cdar(s); // TODO check?
        c.pop(1);
        goto dispatch;

      case instruction::secd::CONS:
        DEBUG_0();
        s = cons(cons(car(s), cadr(s)), cddr(s));
        c.pop();
        goto dispatch;

      case instruction::secd::DEFINE:
        DEBUG_1();
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define (change to #<undefined>?)
        c.pop(2);
        goto dispatch;

      case instruction::secd::STOP: // (result . S) E (STOP . C) D
        DEBUG_0();
        c.pop(1);
        return car(s);

      case instruction::secd::APPLY:
        DEBUG_0();

        if (auto applicable {car(s)}; not applicable)
        {
          throw error {"unit is not appliciable"};
        }
        else if (applicable.is<closure>()) // (closure args . S) E (APPLY . C) D
        {
          d.push(cddr(s), e, cdr(c));
          c = car(applicable);
          e = cons(cadr(s), cdr(applicable));
          s = unit;
        }
        else if (applicable.is<procedure>()) // (procedure args . S) E (APPLY . C) D
        {
          s = cons(applicable.as<procedure>()(cadr(s)), cddr(s));
          c.pop(1);
        }
        else
        {
          throw error {pseudo_display(applicable, "\x1b[31m", " is not applicable")};
        }
        goto dispatch;

      case instruction::secd::RETURN: // (value . S) E (RETURN . C) (S' E' C' . D) => (value . S') E' C' D
        DEBUG_0();
        s = cons(car(s), d.pop());
        e = d.pop();
        c = d.pop();
        goto dispatch;

      case instruction::secd::POP: // (var . S) E (POP . C) D => S E C D
        DEBUG_0();
        s.pop(1);
        c.pop(1);
        goto dispatch;

      case instruction::secd::SETG: // (value . S) E (SETG symbol . C) D => (value . S) E C D
        DEBUG_1();

        if (auto& lhs {unsafe_assoc(cadr(c), env)}; !lhs)
        {
          throw error {pseudo_display(cadr(c), "\x01b[31m", " is unbound")};
        }
        else // TODO ASSIGN
        {
          std::atomic_store(&lhs, car(s).access().copy());
        }

        c.pop(2);
        goto dispatch;

      case instruction::secd::SETL: // (var . S) E (SETG (i . j) . C) D => (var . S) E C D
        {
          DEBUG_1();

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).as<number>()};

          // TODO Add SETV (set-variadic) instruction to remove this conditional.
          auto& tmp {e};

          while (0 < i--)
          {
            tmp = cdr(tmp);
          }

          if (auto& scope {car(tmp)}; j < 0)
          {
            // std::next(scope, -++j) <= car(s);
            auto& var {scope};
            while (++j < -1) // ここ自信ない（一つ多いか少ないかも）
            {
              var = cdr(var);
            }
            std::atomic_store(&var, car(s));
          }
          else
          {
            // car(std::next(scope, j)) <= car(s);
            auto& var {scope};
            while (0 < j--)
            {
              var = cdr(var);
            }
            std::atomic_store(&car(var), car(s));
          }

          c.pop(2);
        }
        goto dispatch;

      default:
        throw error {pseudo_display(car(c), " is not virtual machine instruction")};
      }

      throw error {pseudo_display("unterminated execution")};
    }

    objective begin(const objective& exp,
                    const objective& scope,
                    const objective& continuation)
    {
      return compile(
               car(exp),
               scope,
               cdr(exp) ? cons(POP, begin(cdr(exp), scope, continuation))
                        :                                  continuation
             );
    }

    objective locate(const objective& exp, const objective& scope)
    {
      auto i {0}, j {0};

      for (cursor x {scope}; x; ++x, ++i)
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

  protected: // Compilation Helpers
    bool local_defined(const cursor& exp, const cursor& scope)
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

    objective args(const objective& exp,
                   const objective& scope,
                   const objective& continuation)
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

