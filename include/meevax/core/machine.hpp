#ifndef INCLUDED_MEEVAX_CORE_MACHINE_HPP
#define INCLUDED_MEEVAX_CORE_MACHINE_HPP

#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <numeric>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <utility>

#include <meevax/core/boolean.hpp>
#include <meevax/core/context.hpp>
#include <meevax/core/instruction.hpp>
#include <meevax/core/number.hpp>
#include <meevax/core/operator.hpp>
#include <meevax/core/pair.hpp>
#include <meevax/core/procedure.hpp>

namespace meevax::core
{
  // Simple SECD machine.
  class machine
  {
    cursor s, e, c, d;

    // cursor env; // global environment
    std::unordered_map<cursor, cursor> env;

    // #define END std::flush << "\r\x1B[K"
    #define END std::endl

    #define DEBUG_0() std::cerr << "\x1B[?7l\t" << car(c) << "\x1B[?7h" << END
    #define DEBUG_1() std::cerr << "\x1B[?7l\t" << car(c) << " " << cadr(c) << "\x1B[?7h" << END
    #define DEBUG_2() std::cerr << "\x1B[?7l\t" << car(c) << " " << cadr(c) << " " << caddr(c) << "\x1B[?7h" << END

  public:
    template <typename... Ts>
    decltype(auto) define(const cursor& var, Ts&&... args)
    {
      // return env = list(var, std::forward<Ts>(args)...) | env;
      return env.insert_or_assign(var, std::forward<Ts>(args)...);
    }

    // XXX This sets C++ procedure definition as help message.
    #define DEFINE_PROCEDURE(NAME, ...) \
      define(package->intern(NAME), cursor::bind<procedure>(#__VA_ARGS__, __VA_ARGS__))

    explicit machine(const std::shared_ptr<context>& package)
      // : env {unit}
    {
      DEFINE_PROCEDURE("pair?", [&](const cursor& args)
      {
        assert(0 < std::distance(args, unit));

        for (auto iter {args}; iter; ++iter)
        {
          if (auto exp {*iter}; not exp or not exp.is<pair>())
          {
            return false_v;
          }
        }

        return true_v;
      });

      DEFINE_PROCEDURE("eq?", [&](const cursor& args)
      {
        assert(1 < std::distance(args, unit));
        return car(args) == cadr(args) ? true_v : false_v;
      });

      DEFINE_PROCEDURE("+", [&](const cursor& args)
      {
        return std::accumulate(args, unit, cursor::bind<number>(0), std::plus {});
      });

      DEFINE_PROCEDURE("*", [&](const cursor& args)
      {
        return std::accumulate(args, unit, cursor::bind<number>(1), std::multiplies {});
      });

      // XXX UGLY CODE
      DEFINE_PROCEDURE("-", [&](const cursor& args)
      {
        if (std::distance(args, unit) < 2)
        {
          return std::accumulate(args, unit, cursor::bind<number>(0), std::minus {});
        }
        else
        {
          return std::accumulate(cursor {cdr(args)}, unit, car(args), std::minus {});
        }
      });

      DEFINE_PROCEDURE("/", [&](const cursor& args)
      {
        return std::accumulate(args, unit, cursor::bind<number>(1), std::divides {});
      });
    }

    auto execute(const cursor& exp)
    {
      s = e = d = unit;

      for (c = exp; c; )
      {
        if (const auto& instruction {car(c)}; instruction == LDX) // S E (LDX (i . j) . C) D => (value . S) E C D
        {
          DEBUG_1();

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).data().as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).data().as<number>()};

          // TODO Add LDV (load-variadic) instruction to remove this conditional.
          if (cursor target_stack_frame {car(std::next(e, i))}; j < 0)
          {
            s = cons(std::next(target_stack_frame, -++j), s);
          }
          else
          {
            s = cons(car(std::next(target_stack_frame, j)), s);
          }

          c = cddr(c);
        }
        else if (instruction == LDC) // S E (LDC constant . C) D => (constant . S) E C D
        {
          // XXX Add (LDC unit) combination as new instruction NIL?
          DEBUG_1();
          s = cons(cadr(c), s);
          c = cddr(c);
        }
        else if (instruction == LDG) // S E (LDG symbol . C) D => (value . S) E C D
        {
          DEBUG_1();

          // if (const auto& var {assoc(cadr(c), env)}; var == undefined)
          // {
          //   std::stringstream buffer {};
          //   buffer << cadr(c) << " is undefined variable";
          //   throw std::runtime_error {buffer.str()};
          // }
          // else
          // {
          //   s = cons(var, s);
          // }

          if (auto iter {env.find(cadr(c))}; iter != std::end(env))
          {
            s = cons(iter->second, s);
          }
          else
          {
            std::stringstream buffer {};
            buffer << cadr(c) << " is undefined variable";
            throw std::runtime_error {buffer.str()};
          }

          c = cddr(c);
        }
        else if (instruction == LDF) // S E (LDF code . C) => (closure . S) E C D
        {
          DEBUG_1();
          s = cons(cursor::bind<closure>(cadr(c), e), s);
          c = cddr(c);
        }
        else if (instruction == SELECT) // (boolean . S) E (SELECT then else . C) D => S E then/else (C. D)
        {
          DEBUG_2();
          d = cons(cdddr(c), d);
          c = (car(s) != false_v ? cadr(c) : caddr(c));
          s = cdr(s);
        }
        else if (instruction == JOIN) // S E (JOIN . x) (C . D) => S E C D
        {
          DEBUG_0();
          c = car(d);
          d = cdr(d);
        }
        else if (instruction == CAR)
        {
          DEBUG_0();
          car(s) = caar(s); // TODO check?
          c = cdr(c);
        }
        else if (instruction == CDR)
        {
          DEBUG_0();
          car(s) = cdar(s); // TODO check?
          c = cdr(c);
        }
        else if (instruction == CONS)
        {
          DEBUG_0();
          s = cons(cons(car(s), cadr(s)), cddr(s));
          c = cdr(c);
        }
        else if (instruction == DEFINE)
        {
          DEBUG_1();
          // env = cons(list(cadr(c), car(s)), env);
          define(cadr(c), car(s));
          car(s) = cadr(c);
          c = cddr(c);
        }
        else if (instruction == STOP) // (result . S) E (STOP . C) D
        {
          DEBUG_0();
          c = cdr(c);
          return car(s);
        }
        else if (instruction == APPLY)
        {
          DEBUG_0();

          // XXX Maybe this error won't occur.
          if (auto applicable {car(s)}; not applicable)
          {
            std::stringstream buffer {};
            buffer << applicable << " is not applicable";
            throw std::runtime_error {buffer.str()};
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
            s = cons(applicable.data().as<procedure>()(cadr(s)), cddr(s));
            c = cdr(c);
          }
          else
          {
            std::stringstream buffer {};
            buffer << "unimplemented operator " << applicable;
            throw std::runtime_error {buffer.str()};
          }
        }
        else if (instruction == RETURN)
        {
          DEBUG_0();
          s = cons(car(s), car(d));
          e = cadr(d);
          c = caddr(d);
          d = cdddr(d);
        }
        else
        {
          std::stringstream buffer {};
          buffer << "unknown instruction \"" << instruction << "\"";
          throw std::runtime_error {buffer.str()};
        }
      }

      std::stringstream buffer {};
      buffer << "unterminated machine code executed\n"
             << "terminated:\n"
             << "\ts\t" << s << "\n"
             << "\te\t" << e << "\n"
             << "\tc\t" << c << "\n"
             << "\td\t" << d << "\n";

      throw std::runtime_error {buffer.str()};
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_MACHINE_HPP

