#ifndef INCLUDED_MEEVAX_CORE_MACHINE_HPP
#define INCLUDED_MEEVAX_CORE_MACHINE_HPP

#include <iostream>
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
        if (car(c) == LDX) // S E (LDX (i . j) . C) D
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
      }

      throw std::runtime_error {"unterminated machine code executed"};
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_MACHINE_HPP

