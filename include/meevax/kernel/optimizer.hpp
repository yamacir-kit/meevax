/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_OPTIMIZER_HPP
#define INCLUDED_MEEVAX_KERNEL_OPTIMIZER_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct optimizer
  {
    static inline auto fmerge_constants = true;

    static auto merge_constants(object const& c) -> object
    {
      if (not c.is<pair>())
      {
        return c;
      }
      else switch (c[0].as<instruction>())
      {
      case instruction::load_constant: /* --------------------------------------
      *
      *  (load-constant x
      *   load-constant y
      *   cons
      *   ...)
      *
      *  => (load-constant (x . y)
      *      ...)
      *
      * --------------------------------------------------------------------- */
        if (5 <= length(c) and
            c[0].is<instruction>() and
            c[0].as<instruction>() == instruction::load_constant and
            c[2].is<instruction>() and
            c[2].as<instruction>() == instruction::load_constant and
            c[4].is<instruction>() and
            c[4].as<instruction>() == instruction::cons)
        {
          return merge_constants(cons(c[0],
                                      cons(c[3], c[1]),
                                      merge_constants(tail(c, 5))));
        }
        else if (let const& continuation = merge_constants(cddr(c)); continuation == cddr(c))
        {
          return c;
        }
        else
        {
          return cons(car(c), cadr(c), continuation);
        }

      case instruction::load_closure:
      case instruction::load_continuation:
        if (let const& subcontrol   = merge_constants(cadr(c)),
                       continuation = merge_constants(cddr(c));
            subcontrol == cadr(c) and continuation == cddr(c))
        {
          return c;
        }
        else
        {
          return cons(c[0], subcontrol, continuation);
        }

      case instruction::select:
      case instruction::tail_select:
        if (let const& consequent   = merge_constants(cadr(c)),
                       alternate    = merge_constants(caddr(c)),
                       continuation = merge_constants(cdddr(c));
            consequent == cadr(c) and alternate == caddr(c) and continuation == cdddr(c))
        {
          return c;
        }
        else
        {
          return cons(c[0], consequent, alternate, continuation);
        }

      default:
        {
          auto length = instruction_length(c[0].as<instruction>());

          if (let const& continuation = merge_constants(tail(c, length)); continuation == tail(c, length))
          {
            return c;
          }
          else
          {
            return append2(take(c, length), continuation);
          }
        }
      }
    }

    static auto optimize(object const& c)
    {
      let code = c;

      if (fmerge_constants)
      {
        code = merge_constants(code);
      }

      return code;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OPTIMIZER_HPP
