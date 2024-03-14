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

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/optimizer.hpp>

namespace meevax
{
inline namespace kernel
{
  auto merge_constants(object & c) -> void
  {
    assert(c.is<pair>());

    assert(car(c).is<instruction>());

    switch (car(c).as<instruction>())
    {
    case instruction::load_constant: /* ----------------------------------------
    *
    *  (load-constant x
    *   load-constant y
    *   cons
    *   ...)
    *
    *  => (load-constant (y . x)
    *      ...)
    *
    * ----------------------------------------------------------------------- */
      if (cddr(c).is<pair>() and
          caddr(c).is<instruction>() and caddr(c).as<instruction>() == instruction::load_constant and
          cddddr(c).is<pair>() and
          caddddr(c).is<instruction>() and caddddr(c).as<instruction>() == instruction::cons)
      {
        cadr(c) = cons(cadddr(c), cadr(c));
        cddr(c) = cdddddr(c);
        merge_constants(c);
      }
      else
      {
        merge_constants(cddr(c));
      }
      break;

    case instruction::join:
    case instruction::tail_call:
    case instruction::tail_letrec:
    case instruction::return_:
    case instruction::stop:
      assert(cdr(c).is<null>());
      break;

    case instruction::call:
    case instruction::cons:
    case instruction::drop:
    case instruction::dummy:
    case instruction::letrec:
      merge_constants(cdr(c));
      break;

    case instruction::current:
    case instruction::install:
    case instruction::load_absolute:
    case instruction::load_relative:
    case instruction::load_variadic:
    case instruction::store_absolute:
    case instruction::store_relative:
    case instruction::store_variadic:
      merge_constants(cddr(c));
      break;

    case instruction::load_closure:
    case instruction::load_continuation:
      merge_constants(cadr(c));
      merge_constants(cddr(c));
      break;

    case instruction::select:
      merge_constants(cadr(c));
      merge_constants(caddr(c));
      merge_constants(cdddr(c));
      break;

    case instruction::tail_select:
      assert(cdddr(c).is<null>());
      merge_constants(cadr(c));
      merge_constants(caddr(c));
      break;
    }
  }

  auto optimize(object code) -> object
  {
    merge_constants(code);

    return code;
  }
} // namespace kernel
} // namespace meevax
