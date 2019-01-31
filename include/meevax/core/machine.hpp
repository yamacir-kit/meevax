#ifndef INCLUDED_MEEVAX_CORE_MACHINE_HPP
#define INCLUDED_MEEVAX_CORE_MACHINE_HPP

#include <iostream>
#include <stdexcept>

#include <meevax/core/boolean.hpp>
#include <meevax/core/context.hpp>
#include <meevax/core/cursor.hpp>
#include <meevax/core/instruction.hpp>
#include <meevax/core/operator.hpp>

namespace meevax::core
{
  class machine
  {
    cursor s, e, c, d;
    cursor env;

    #define debug_operand0() std::cerr << "\x1B[?7l\t" << car(c) << "\x1B[?7h" << std::endl;
    #define debug_operand1() std::cerr << "\x1B[?7l\t" << car(c) << " " << cadr(c) << "\x1B[?7h" << std::endl;
    #define debug_operand2() std::cerr << "\x1B[?7l\t" << car(c) << " " << cadr(c) << " " << caddr(c) << "\x1B[?7h" << std::endl;

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
          debug_operand0();
        }
      }

      throw std::runtime_error {"unterminated machine code executed"};
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_MACHINE_HPP

