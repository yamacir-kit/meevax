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

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/syntactic_closure.hpp>
#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  syntactic_closure::renamer::renamer(syntactic_closure const* enclosure, renamer * outer, bool transparent)
    : enclosure   { enclosure }
    , outer       { outer }
    , transparent { transparent }
  {
    assert(enclosure);
  }

  auto syntactic_closure::renamer::count(let const& form) -> int
  {
    assert(form.is<symbol>());

    return (outer ? outer->count(form) : 0) + std::count_if(dictionary.begin(), dictionary.end(), [&](let const& entry)
                                                            {
                                                              return eq(car(entry), form);
                                                            });
  }

  auto syntactic_closure::renamer::unshadow(let const& formals,
                                            let const& bound_variables) -> object
  {
    auto rename = [&](let const& form)
    {
      assert(form.is_also<identifier>() or form.is<macro>() or form.is<null>());

      if (form.is<symbol>() and std::any_of(bound_variables.begin(), bound_variables.end(), [&](let const& formals)
                                            {
                                              return meevax::memq(form, formals); // TODO variadic arguments
                                            }))
      {
        return make_syntactic_closure(form, count(form) + 1);
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

  auto syntactic_closure::renamer::memq(let const& form) const -> object
  {
    if (let const& x = meevax::memq(form, enclosure->free_names); x != f)
    {
      return x;
    }
    else
    {
      return transparent and outer ? outer->memq(form) : f;
    }
  }

  auto syntactic_closure::renamer::assq(let const& form) const -> object
  {
    if (let const& x = meevax::assq(form, dictionary); x != f)
    {
      return x;
    }
    else
    {
      return transparent and outer ? outer->assq(form) : f;
    }
  }

  auto syntactic_closure::renamer::make_syntactic_closure(let const& form, int version) -> object const&
  {
    return cdar(dictionary = alist_cons(form,
                                        make<syntactic_closure>(enclosure->environment, unit, form, version),
                                        dictionary));
  }

  auto syntactic_closure::renamer::rename(let const& form) -> object
  {
    assert(form.is_also<identifier>() or form.is<macro>() or form.is<null>());

    auto inject = [this](let const& form)
    {
      return outer ? outer->rename(form) : form;
    };

    if (form.is<symbol>())
    {
      if (memq(form) != f)
      {
        return inject(form);
      }
      else if (let const& renaming = assq(form); renaming != f)
      {
        return cdr(renaming);
      }
      else
      {
        return transparent ? inject(form) : make_syntactic_closure(form);
      }
    }
    else
    {
      return form;
    }
  }

  auto syntactic_closure::renamer::operator ()(let const& form) -> object
  {
    return rename(form);
  }

  auto syntactic_closure::renamer::operator ()(let const& formals,
                                               let const& bound_variables) -> object
  {
    return unshadow(formals, bound_variables);
  }

  syntactic_closure::syntactic_closure(let const& environment,
                                       let const& free_names,
                                       let const& form,
                                       int version)
    : environment { environment }
    , free_names  { free_names }
    , form        { form }
    , version     { version }
  {
    assert(environment.is<syntactic_environment>());
  }

  auto syntactic_closure::expand(let const& bound_variables, renamer & outer) -> object
  {
    auto rename = renamer(this,
                          &outer,
                          eq(environment.template as<syntactic_environment>().first,
                             bound_variables));

    return environment.as<syntactic_environment>().expand(form, bound_variables, rename);
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

      return environment.as_const<syntactic_environment>().identify(form, xs);
    };

    if (let const& identity = identify(); identity != f)
    {
      return identity;
    }
    else
    {
      return environment.as_const<syntactic_environment>().identify(form, bound_variables);
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
    if (datum.form.template is_also<identifier>())
    {
      if (0 < datum.version)
      {
        return os << datum.form << ':' << datum.version;
      }
      else
      {
        return os << '$' << datum.form;
      }
    }
    else
    {
      return os << datum.form;
    }
  }
} // namespace meevax::kernel
