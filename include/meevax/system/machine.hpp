#ifndef INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP

#include <functional> // std::invoke

#include <meevax/system/boolean.hpp> // false_v
#include <meevax/system/closure.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/instruction.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/special.hpp>
#include <meevax/system/srfi-1.hpp> // assoc
#include <meevax/system/symbol.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax::system
{
  template <typename Enclosure>
  class machine // Simple SECD machine.
  {
  protected: // XXX TO PRIVATE
    cursor s, // stack
           e, // lexical environment
           c, // code
           d; // dump

    #define DEBUG_0() // std::cerr << ";\x1B[?7l\t" << take(c, 1) << "\x1B[?7h" << std::endl
    #define DEBUG_1() // std::cerr << ";\x1B[?7l\t" << take(c, 2) << "\x1B[?7h" << std::endl
    #define DEBUG_2() // std::cerr << ";\x1B[?7l\t" << take(c, 3) << "\x1B[?7h" << std::endl

  public:
    decltype(auto) interaction_environment()
    {
      return static_cast<Enclosure&>(*this).interaction_environment();
    }

    // Direct virtual machine instruction invocation.
    template <typename... Ts>
    decltype(auto) define(const objective& key, Ts&&... args)
    {
    #if 0
      return interaction_environment().push(list(key, std::forward<Ts>(args)...));
    #else
      interaction_environment().push(list(key, std::forward<Ts>(args)...));
      std::cout << "\t" << caar(interaction_environment()) << "\r\x1b[40C " << cadar(interaction_environment()) << std::endl;
      return interaction_environment();
    #endif
    }

    objective compile(const objective& exp,
                      const objective& scope = unit,
                      const objective& continuation = list(STOP))
    {
      if (not exp)
      {
        // TRACE("compile") << " ; => is quoted unit or list termination" << std::endl;
        return cons(LDC, unit, continuation);
      }
      else if (not exp.is<pair>())
      {
        TRACE("compile") << exp << " ; => ";

        if (exp.is<symbol>()) // is variable
        {
          if (auto location {locate(exp, scope)}; location) // there is local-defined variable
          {
            // load variable value (bound to lambda parameter) at runtime
            std::cerr << "is local variable => " << list(LDX, location) << std::endl;
            return cons(LDX, location, continuation);
          }
          else
          {
            // load variable value from global-environment at runtime
            std::cerr << "is global variable => " << list(LDG, exp) << std::endl;
            return cons(LDG, exp, continuation);
          }
        }
        else // is self-evaluation
        {
          std::cerr << "is self-evaluation => " << list(LDC, exp) << std::endl;
          return cons(LDC, exp, continuation);
        }
      }
      else // is (application . arguments)
      {
        if (const objective& buffer {assoc(car(exp), interaction_environment())};
            /* std::cerr << "." << std::flush, */ !buffer)
        {
          TRACE("compile") << "(" << car(exp) << " ; => is application of unit => ERROR" << std::endl;
          throw error {"unit is not applicable"};
        }
        else if ((/* std::cerr << "." << std::flush, */ buffer != unbound) &&
                 (/* std::cerr << "." << std::flush, */ buffer.is<special>()) &&
                 (/* std::cerr << "." << std::flush, */ not locate(car(exp), scope)))
        {
          TRACE("compile") << "(" << car(exp) << " ; => is application of ";
          std::cerr << buffer << std::endl;
          NEST_IN;
          // XXX 何故かスペシャルフォームが引数じゃなくて自分自身も受け取るスタイルになってる、cdr(exp) だけ受け取れば十分
          auto result {std::invoke(buffer.as<special>(), exp, scope, continuation)};
          NEST_OUT;
          return result;
        }
        else if ((/* std::cerr << "." << std::flush, */ buffer != unbound) &&
                 (/* std::cerr << "." << std::flush, */ buffer.is<Enclosure>()) &&
                 (/* std::cerr << "." << std::flush, */ not locate(car(exp), scope)))
        {
          TRACE("compile") << "(" << car(exp) << " ; => is use of " << buffer << " => " << std::flush;

          auto& macro {unsafe_assoc(car(exp), interaction_environment()).template as<Enclosure&>()};
          // auto expanded {macro.expand(cdr(exp), interaction_environment())};
          auto expanded {macro.expand(cdr(exp))};
          TRACE("expanded") << expanded << std::endl;

          NEST_IN;
          auto result {compile(expanded, scope, continuation)};
          NEST_OUT;
          return result;
        }
        else // is (closure . arguments)
        {
          TRACE("compile") << "( ; => is any application " << std::endl;

          NEST_IN;
          auto result {args(
                   cdr(exp),
                   scope,
                   compile(car(exp), scope, cons(APPLY, continuation))
                 )};
          NEST_OUT;
          return result;
        }
      }
    }

    decltype(auto) execute(const objective& exp)
    {
      c = exp;
      return execute();
    }

    objective execute()
    {
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
        }
        c.pop(2);
        goto dispatch;

      case instruction::secd::LDC: // S E (LDC constant . C) D => (constant . S) E C D
        DEBUG_1();
        s.push(cadr(c));
        c.pop(2);
        goto dispatch;

      case instruction::secd::LDG: // S E (LDG symbol . C) D => (value . S) E C D
        DEBUG_1();
        if (auto value {assoc(cadr(c), interaction_environment())}; value != unbound)
        {
          s.push(value);
        }
        else
        {
          throw error {cadr(c), " is unbound"};
        }
        c.pop(2);
        goto dispatch;

      case instruction::secd::LDS: // S E (LDS code . C) => (enclosure . S) E C D
        DEBUG_1();
        s.push(make<Enclosure>(cadr(c), interaction_environment())); // レキシカル環境が必要ないのかはよく分からん
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
        c = car(s) != false_v ? cadr(c) : caddr(c);
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
        s = cons(cons(car(s), cadr(s)), cddr(s)); // s = car(s) | cadr(s) | cddr(s);
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
        // return car(s);
        return s.pop();

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
        else if (applicable.is<procedure>())
          //    (procedure args . S) E (APPLY . C) D
          // =>         (result . S) E          C  D
        {
          s = std::invoke(applicable.as<procedure>(), cadr(s)) | cddr(s);
          c.pop(1);
        }
        else
        {
          throw error {applicable, "\x1b[31m", " is not applicable"};
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
        // TODO
        // (1) 右辺値がユニークな場合はコピーを作らなくても問題ない
        // (2) 左辺値がユニークな場合は直接書き換えても問題ない
        // (3) 右辺値が左辺値よりも新しい場合は弱参照をセットしなければならない
        std::atomic_store(&unsafe_assoc(cadr(c), interaction_environment()), car(s).access().copy());
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
        }
        c.pop(2);
        goto dispatch;

      default:
        throw error {car(c), "\x1b[31m is not virtual machine instruction"};
      }

      throw error {"unterminated execution"};
    }

    // template <typename... Ts>
    // decltype(auto) operator()(Ts&&... args)
    // {
    //   return execute(std::forward<Ts>(args)...);
    // }

  protected:
    // 名前をbeginにしたいけどSTLと被る
    objective body(const objective& exp,
                   const objective& scope,
                   const objective& continuation)
    {
      return compile(
               car(exp),
               scope,
               cdr(exp) ? cons(POP, body(cdr(exp), scope, continuation))
                        :                                 continuation
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
    [[deprecated]] bool defined(const cursor& exp, const cursor& scope)
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

