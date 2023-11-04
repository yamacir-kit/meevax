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
      if (not c.is<pair>() or not car(c).is<instruction>())
      {
        return c;
      }
      else switch (auto n = size(car(c).as<instruction>()); car(c).as<instruction>())
      {
      case instruction::load_constant: /* --------------------------------------
      *
      *  (load-constant x
      *   load-constant y
      *   cons
      *   ...)
      *
      *  => (load-constant (y . x)
      *      ...)
      *
      * --------------------------------------------------------------------- */
        if (tail(c, 2).is<pair>() and head(c, 2).is<instruction>() and head(c, 2).as<instruction>() == instruction::load_constant and
            tail(c, 4).is<pair>() and head(c, 4).is<instruction>() and head(c, 4).as<instruction>() == instruction::cons)
        {
          return merge_constants(cons(head(c, 0),
                                      cons(head(c, 3),
                                           head(c, 1)),
                                      merge_constants(tail(c, 5))));
        }
        else if (let const& c2 = merge_constants(tail(c, 2)); c2 == tail(c, 2))
        {
          return c;
        }
        else
        {
          return cons(head(c, 0), head(c, 1), c2);
        }

      default:
        if (let const& cn = merge_constants(tail(c, n)); cn == tail(c, n))
        {
          return c;
        }
        else
        {
          return append(take(c, n), cn);
        }

      case instruction::load_closure:
      case instruction::load_continuation:
        if (let const& c1 = merge_constants(head(c, 1)),
                       c2 = merge_constants(tail(c, 2));
            c1 == head(c, 1) and
            c2 == tail(c, 2))
        {
          return c;
        }
        else
        {
          return cons(head(c, 0), c1, c2);
        }

      case instruction::select:
      case instruction::tail_select:
        if (let const& c1 = merge_constants(head(c, 1)),
                       c2 = merge_constants(head(c, 2)),
                       c3 = merge_constants(tail(c, 3));
            c1 == head(c, 1) and
            c2 == head(c, 2) and
            c3 == tail(c, 3))
        {
          return c;
        }
        else
        {
          return cons(head(c, 0), c1, c2, c3);
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
