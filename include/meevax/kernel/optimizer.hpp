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

    static auto merge_constants(const_reference c) -> value_type
    {
      if (not c.is<pair>())
      {
        return c;
      }
      else switch (car(c).template as<mnemonic>())
      {
      case mnemonic::call:
      case mnemonic::cons:
      case mnemonic::drop:
      case mnemonic::dummy:
      case mnemonic::join:
      case mnemonic::letrec:
      case mnemonic::load_r0:
      case mnemonic::return_:
      case mnemonic::stop:
      case mnemonic::store_r0:
      case mnemonic::tail_call:
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

      case mnemonic::define:
      case mnemonic::define_syntax:
      case mnemonic::let_syntax:
      case mnemonic::letrec_syntax:
      case mnemonic::load_absolute:
      case mnemonic::load_relative:
      case mnemonic::load_variadic:
      case mnemonic::store_absolute:
      case mnemonic::store_relative:
      case mnemonic::store_variadic:
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

      case mnemonic::load_closure:
      case mnemonic::load_continuation:
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

      case mnemonic::select:
      case mnemonic::tail_select:
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


      case mnemonic::load_constant: /* -----------------------------------------
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
            list_ref(c, 0).is<mnemonic>() and
            list_ref(c, 0).as<mnemonic>() == mnemonic::load_constant and
            list_ref(c, 2).is<mnemonic>() and
            list_ref(c, 2).as<mnemonic>() == mnemonic::load_constant and
            list_ref(c, 4).is<mnemonic>() and
            list_ref(c, 4).as<mnemonic>() == mnemonic::cons)
        {
          return merge_constants(cons(list_ref(c, 0), cons(list_ref(c, 3),
                                                           list_ref(c, 1)),
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

    static auto optimize(const_reference c)
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
