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
      else switch (car(c).template as<instruction>())
      {
      case instruction::call:
      case instruction::cons:
      case instruction::drop:
      case instruction::dummy:
      case instruction::join:
      case instruction::letrec:
      case instruction::return_:
      case instruction::stop:
      case instruction::tail_call:
        return [&]()
        {
          if (let const& continuation = merge_constants(cdr(c)); continuation == cdr(c))
          {
            return c;
          }
          else
          {
            return cons(car(c), continuation);
          }
        }();

      case instruction::define:
      case instruction::define_syntax:
      case instruction::let_syntax:
      case instruction::letrec_syntax:
      case instruction::load_absolute:
      case instruction::load_auxiliary:
      case instruction::load_relative:
      case instruction::load_variadic:
      case instruction::store_absolute:
      case instruction::store_auxiliary:
      case instruction::store_relative:
      case instruction::store_variadic:
        return [&]()
        {
          if (let const& continuation = merge_constants(cddr(c)); continuation == cddr(c))
          {
            return c;
          }
          else
          {
            return cons(car(c), cadr(c), continuation);
          }
        }();

      case instruction::load_closure:
      case instruction::load_continuation:
        return [&]()
        {
          if (let const& branch       = merge_constants(cadr(c)),
                         continuation = merge_constants(cddr(c));
              branch == cadr(c) and continuation == cddr(c))
          {
            return c;
          }
          else
          {
            return cons(car(c), branch, continuation);
          }
        }();

      case instruction::select:
      case instruction::tail_select:
        return [&]()
        {
          if (let const& consequent   = merge_constants(cadr(c)),
                         alternate    = merge_constants(caddr(c)),
                         continuation = merge_constants(cdddr(c));
              consequent == cadr(c) and alternate == caddr(c) and continuation == cdddr(c))
          {
            return c;
          }
          else
          {
            return cons(car(c), consequent, alternate, continuation);
          }
        }();


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
          return merge_constants(cons(c[0], cons(c[3], c[1]),
                                      merge_constants(list_tail(c, 5))));
        }
        else if (let const& continuation = merge_constants(cddr(c)); continuation == cddr(c))
        {
          return c;
        }
        else
        {
          return cons(car(c), cadr(c), continuation);
        }

      default:
        assert(false);
        return c;
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
