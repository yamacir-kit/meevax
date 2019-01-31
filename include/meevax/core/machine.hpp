#ifndef INCLUDED_MEEVAX_CORE_MACHINE_HPP
#define INCLUDED_MEEVAX_CORE_MACHINE_HPP

#include <iostream>
#include <sstream>
#include <stdexcept>

#include <meevax/core/boolean.hpp>
#include <meevax/core/context.hpp>
#include <meevax/core/cursor.hpp>
#include <meevax/core/instruction.hpp>
#include <meevax/core/number.hpp>
#include <meevax/core/operator.hpp>

namespace meevax::core
{
  // Simple SECD machine.
  class machine
  {
    cursor s, e, c, d;
    cursor env; // global environment

    #define DEBUG_0() std::cerr << "\x1B[?7l\t" << car(c) << "\x1B[?7h" << std::endl;
    #define DEBUG_1() std::cerr << "\x1B[?7l\t" << car(c) << " " << cadr(c) << "\x1B[?7h" << std::endl;
    #define DEBUG_2() std::cerr << "\x1B[?7l\t" << car(c) << " " << cadr(c) << " " << caddr(c) << "\x1B[?7h" << std::endl;

  public:
    explicit machine(const std::shared_ptr<context>& package)
      : env {nil}
    {}

    auto execute(const cursor& exp)
    {
      s = e = d = nil;

      for (c = exp; c; )
      {
        if (const auto& instruction {car(c)}; instruction == LDX) // S E (LDX (i . j) . C) D => (value . S) E C D
        {
          DEBUG_0();

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).data().as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).data().as<number>()};

          // TODO Add LDV (load-variadic) instruction to remove this conditional.
          if (auto target_stack_frame {car(std::next(e, i))}; j < 0)
          {
            s = cons(std::next(target_stack_frame, -++j), s);
          }
          else
          {
            s = cons(car(std::next(target_stack_frame, j), s);
          }

          c = cddr(c);
        }
        else if (instruction == LDC) // S E (LDC constant . C) D => (constant . S) E C D
        {
          // XXX Add (LDC nil) combination as new instruction NIL?
          DEBUG_1();
          s = cons(cadr(c), s);
          c = cddr(c);
        }
        else if (instruction == LDG) // S E (LDG symbol . C) D => (value . S) E C D
        {
          DEBUG_1();
          s = cons(assoc(cadr(c), env), s);
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
          env = cons(list(cadr(c), car(s)), env);
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
          DEBUG_1();

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
            s = nil;
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

