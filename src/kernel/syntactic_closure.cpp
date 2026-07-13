/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <algorithm>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/syntactic_closure.hpp>
#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  syntactic_closure::alpha::alpha(syntactic_closure const* enclosure, alpha * outer)
    : enclosure   { enclosure }
    , outer       { outer }
  {
    assert(enclosure);
  }

  auto syntactic_closure::alpha::unshadow(let const& formals, let const& bound_variables) -> object
  {
    auto rename = [&](let const& form)
    {
      assert(form.is_also<identifier>() or form.is<macro>() or form.is<null>());

      if (form.is<symbol>() and std::ranges::any_of(bound_variables | as_proper_list, [&](let const& formals)
                                                    {
                                                      return static_cast<bool>(memq(form, formals)); // TODO variadic arguments
                                                    }))
      {
        return make_syntactic_closure(form, length(bound_variables));
      }
      else
      {
        return form;
      }
    };

    if (formals.is<pair>())
    {
      return cons(rename(car(formals)), unshadow(cdr(formals), bound_variables));
    }
    else
    {
      return rename(formals);
    }
  }

  auto syntactic_closure::alpha::make_syntactic_closure(let const& form, int level) -> object const&
  {
    assert(form.is<symbol>());

    return cdar(dictionary = alist_cons(form, make<syntactic_closure>(enclosure->environment, unit, form, level), dictionary));
  }

  auto syntactic_closure::alpha::rename(let const& form) -> object
  {
    assert(form.is_also<identifier>() or form.is<macro>() or form.is<null>());

    if (form.is<symbol>())
    {
      if (memq(form, enclosure->free_names) != f)
      {
        return outer ? outer->rename(form) : form;
      }
      else if (let const& renaming = assq(form, dictionary); renaming != f)
      {
        return cdr(renaming);
      }
      else
      {
        return outer ? make_syntactic_closure(form, -1) : form;
      }
    }
    else
    {
      return form;
    }
  }

  syntactic_closure::syntactic_closure(let const& environment,
                                       let const& free_names,
                                       let const& form,
                                       int level)
    : environment { environment }
    , free_names  { free_names }
    , form        { form }
    , level       { level }
  {
    assert(environment.is<syntactic_environment>());
  }

  auto syntactic_closure::expand(let const& bound_variables, alpha & outer) -> object
  {
    if (eq(environment.as<syntactic_environment>().first, bound_variables))
    {
      return environment.as<syntactic_environment>().expand(form, bound_variables, outer);
    }
    else
    {
      auto inner = alpha(this, &outer);

      return environment.as<syntactic_environment>().expand(form, bound_variables, inner);
    }
  }

  auto syntactic_closure::identify(let const& bound_variables) -> object
  {
    auto identify = [&]()
    {
      let xs = environment.as<syntactic_environment>().first;

      for (auto offset = length(bound_variables) - length(xs); 0 < offset; --offset)
      {
        xs = cons(unit, xs);
      }

      return environment.as<syntactic_environment const>().identify(form, xs);
    };

    if (let const& identity = identify(); identity != f)
    {
      return identity;
    }
    else
    {
      return environment.as<syntactic_environment const>().identify(form, bound_variables);
    }
  }

  auto operator ==(syntactic_closure const& x,
                   syntactic_closure const& y) -> bool
  {
    /*
       (free-identifier=? id-1 id-2)                              procedure

       Returns #t if the original occurrences of id-1 and id-2 have the same
       binding, otherwise returns #f. free-identifier=? is used to look for a
       literal identifier in the argument to a transformer, such as else in a
       cond clause. A macro definition for syntax-rules would use
       free-identifier=? to look for literals in the input.
    */
    return x.form.template is_also<identifier>() and
           y.form.template is_also<identifier>() and
           eqv(x.environment.template as<syntactic_environment>().identify(x.form, x.environment.template as<syntactic_environment>().first),
               y.environment.template as<syntactic_environment>().identify(y.form, y.environment.template as<syntactic_environment>().first));
  }

  auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &
  {
    if (datum.form.template is_also<identifier>() and datum.level != 0)
    {
      return os << "<" << datum.form << '%' << datum.level << ">"; // Is automatically introduced by α-conversion.
    }
    else
    {
      return os << "<" << datum.form << ">";
    }
  }
} // namespace meevax::kernel
