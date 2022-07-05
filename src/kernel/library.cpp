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

#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  library::library(syntax_library_t)
  {
    define<syntax>("begin", machine::begin);
    define<syntax>("call-with-current-continuation!", call_with_current_continuation);
    define<syntax>("define", machine::define);
    define<syntax>("define-syntax", define_syntax);
    define<syntax>("if", if_);
    define<syntax>("lambda", lambda);
    define<syntax>("let-syntax", let_syntax);
    define<syntax>("letrec", letrec);
    define<syntax>("letrec-syntax", letrec_syntax);
    define<syntax>("quote", quote);
    define<syntax>("quote-syntax", quote_syntax);
    define<syntax>("set!", set);

    export_("begin");
    export_("call-with-current-continuation!");
    export_("define");
    export_("define-syntax");
    export_("if");
    export_("lambda");
    export_("let-syntax");
    export_("letrec");
    export_("letrec-syntax");
    export_("quote");
    export_("quote-syntax");
    export_("set!");
  }

  library::library(environment_library_t)
  {
    define<procedure>("environment", [](let const& xs)
    {
      let const e = make<environment>();

      for (let const& x : xs)
      {
        e.as<environment>().import_(x);
      }

      return e;
    });

    define<procedure>("%load", [](let const& xs)
    {
      return car(xs).as<environment>().load(cadr(xs).as<string>());
    });

    define<procedure>("interaction-environment", [](auto&&...)
    {
      return interaction_environment();
    });

    export_("environment");
    export_("interaction-environment");
    export_("%load");
  }

  library::library(equivalence_library_t)
  {
    define<predicate>("eq?",  [](let const& xs) { return eq (car(xs), cadr(xs)); });
    define<predicate>("eqv?", [](let const& xs) { return eqv(car(xs), cadr(xs)); });

    export_("eq?");
    export_("eqv?");
  }

  library::library(number_library_t)
  {
    define<predicate>("number?",        [](let const& xs) { return car(xs).is_also<number>(); });
    define<predicate>("complex?",       [](let const& xs) { return car(xs).is_also<number>() and car(xs).as<number>().is_complex (); });
    define<predicate>("real?",          [](let const& xs) { return car(xs).is_also<number>() and car(xs).as<number>().is_real    (); });
    define<predicate>("rational?",      [](let const& xs) { return car(xs).is_also<number>() and car(xs).as<number>().is_rational(); });
    define<predicate>("integer?",       [](let const& xs) { return car(xs).is_also<number>() and car(xs).as<number>().is_integer (); });
    define<predicate>("exact-integer?", [](let const& xs) { return car(xs).is<exact_integer>(); });
    define<predicate>("%complex?",      [](let const& xs) { return car(xs).is<complex      >(); });
    define<predicate>("ratio?",         [](let const& xs) { return car(xs).is<ratio        >(); });
    define<predicate>("single-float?",  [](let const& xs) { return car(xs).is<single_float >(); });
    define<predicate>("double-float?",  [](let const& xs) { return car(xs).is<double_float >(); });

    #define DEFINE(SYMBOL, COMPARE)                                            \
    define<predicate>(#SYMBOL, [](let const& xs)                               \
    {                                                                          \
      return std::adjacent_find(                                               \
               std::begin(xs), std::end(xs), [](let const& a, let const& b)    \
               {                                                               \
                 return not COMPARE(a.as<number>(), b);                        \
               }) == std::end(xs);                                             \
    })

    DEFINE(= , std::equal_to     <void>());
    DEFINE(!=, std::not_equal_to <void>());
    DEFINE(< , std::less         <void>());
    DEFINE(<=, std::less_equal   <void>());
    DEFINE(> , std::greater      <void>());
    DEFINE(>=, std::greater_equal<void>());

    #undef DEFINE

    define<procedure>("+", [](let const& xs) { return std::accumulate(std::begin(xs), std::end(xs), e0, std::plus<void>()); });
    define<procedure>("*", [](let const& xs) { return std::accumulate(std::begin(xs), std::end(xs), e1, std::multiplies<void>()); });

    #define DEFINE(SYMBOL, FUNCTION, BASIS)                                    \
    define<procedure>(SYMBOL, [](let const& xs)                                \
    {                                                                          \
      switch (length(xs))                                                      \
      {                                                                        \
      case 0:                                                                  \
        throw invalid_application(intern(SYMBOL) | xs);                        \
                                                                               \
      case 1:                                                                  \
        return FUNCTION(BASIS, car(xs));                                       \
                                                                               \
      default:                                                                 \
        return std::accumulate(                                                \
                 std::next(std::begin(xs)), std::end(xs), car(xs),             \
                 [](let const& a, let const& b)                                \
                 {                                                             \
                   return FUNCTION(a, b);                                      \
                 });                                                           \
      }                                                                        \
    })

    DEFINE("-", sub, e0);
    DEFINE("/", div, e1);
    DEFINE("%", mod, e1);

    #undef DEFINE

    define<procedure>("floor",    [](let const& xs) { return car(xs).as<number>().floor  (); });
    define<procedure>("ceiling",  [](let const& xs) { return car(xs).as<number>().ceil   (); });
    define<procedure>("truncate", [](let const& xs) { return car(xs).as<number>().trunc  (); });
    define<procedure>("round",    [](let const& xs) { return car(xs).as<number>().round  (); });

    define<procedure>("exact",    [](let const& xs) { return car(xs).as<number>().exact  (); });
    define<procedure>("inexact",  [](let const& xs) { return car(xs).as<number>().inexact(); });

    define<procedure>("expt", [](let const& xs)
    {
      return car(xs).as<number>().pow(cadr(xs));
    });

    define<procedure>("integer->char", [](let const& xs)
    {
      if (xs.is<pair>() and car(xs).is<exact_integer>())
      {
        return make<character>(static_cast<character::value_type>(car(xs).as<exact_integer>()));
      }
      else
      {
        throw invalid_application(intern("integer->char") | xs);
      }
    });

    define<procedure>("number->string", [](auto&& xs)
    {
      return make<string>(lexical_cast<external_representation>(car(xs)));
    });

    export_("number?");
    export_("complex?");
    export_("real?");
    export_("rational?");
    export_("integer?");
    export_("exact-integer?");
    export_("%complex?");
    export_("ratio?");
    export_("single-float?");
    export_("double-float?");
    export_("=");
    export_("!=");
    export_("<");
    export_("<=");
    export_(">");
    export_(">=");
    export_("+");
    export_("*");
    export_("-");
    export_("/");
    export_("%");
    export_("floor");
    export_("ceiling");
    export_("truncate");
    export_("round");
    export_("expt");
    export_("exact");
    export_("inexact");
    export_("integer->char");
    export_("number->string");
  }

  library::library(inexact_library_t)
  {
    define<predicate>("finite?",   [](let const& xs) { return car(xs).as<number>().is_finite  (); });
    define<predicate>("infinite?", [](let const& xs) { return car(xs).as<number>().is_infinite(); });

    define<predicate>("nan?", [](let const& xs)
    {
      return car(xs).is_also<number>() and car(xs).as<number>().is_nan();
    });

    define<procedure>("exp",  [](let const& xs) { return car(xs).as<number>().exp();  });
    define<procedure>("sqrt", [](let const& xs) { return car(xs).as<number>().sqrt(); });

    define<procedure>("log", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<number>().log();

      case 2:
        return car(xs).as<number>().log() / cadr(xs).as<number>().log();

      default:
        throw invalid_application(intern("log") | xs);
      }
    });

    define<procedure>("sin",   [](let const& xs) { return car(xs).as<number>().sin();   });
    define<procedure>("cos",   [](let const& xs) { return car(xs).as<number>().cos();   });
    define<procedure>("tan",   [](let const& xs) { return car(xs).as<number>().tan();   });
    define<procedure>("asin",  [](let const& xs) { return car(xs).as<number>().asin();  });
    define<procedure>("acos",  [](let const& xs) { return car(xs).as<number>().acos();  });
    define<procedure>("sinh",  [](let const& xs) { return car(xs).as<number>().sinh();  });
    define<procedure>("cosh",  [](let const& xs) { return car(xs).as<number>().cosh();  });
    define<procedure>("tanh",  [](let const& xs) { return car(xs).as<number>().tanh();  });
    define<procedure>("asinh", [](let const& xs) { return car(xs).as<number>().asinh(); });
    define<procedure>("acosh", [](let const& xs) { return car(xs).as<number>().acosh(); });
    define<procedure>("atanh", [](let const& xs) { return car(xs).as<number>().atanh(); });

    define<procedure>("atan", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<number>().atan();

      case 2:
        return car(xs).as<number>().atan2(cadr(xs));

      default:
        throw invalid_application(intern("atan") | xs);
      }
    });

    export_("finite?");
    export_("infinite?");
    export_("nan?");
    export_("exp");
    export_("sqrt");
    export_("log");
    export_("sin");
    export_("asin");
    export_("sinh");
    export_("asinh");
    export_("cos");
    export_("acos");
    export_("cosh");
    export_("acosh");
    export_("tan");
    export_("atan");
    export_("tanh");
    export_("atanh");
  }

  library::library(pair_library_t)
  {
    define<predicate>("pair?", [](let const& xs)
    {
      return car(xs).is<pair>();
    });

    define<intrinsic>("cons", cons_, [](let const& xs)
    {
      return cons(car(xs), cadr(xs));
    });

    define<procedure>("car", [](let const& xs) { return caar(xs); });
    define<procedure>("cdr", [](let const& xs) { return cdar(xs); });

    define<procedure>("caar", [](let const& xs) { return caar(car(xs)); });
    define<procedure>("cadr", [](let const& xs) { return cadr(car(xs)); });
    define<procedure>("cdar", [](let const& xs) { return cdar(car(xs)); });
    define<procedure>("cddr", [](let const& xs) { return cddr(car(xs)); });

    define<procedure>("caaar", [](let const& xs) { return caaar(car(xs)); });
    define<procedure>("caadr", [](let const& xs) { return caadr(car(xs)); });
    define<procedure>("cadar", [](let const& xs) { return cadar(car(xs)); });
    define<procedure>("caddr", [](let const& xs) { return caddr(car(xs)); });
    define<procedure>("cdaar", [](let const& xs) { return cdaar(car(xs)); });
    define<procedure>("cdadr", [](let const& xs) { return cdadr(car(xs)); });
    define<procedure>("cddar", [](let const& xs) { return cddar(car(xs)); });
    define<procedure>("cdddr", [](let const& xs) { return cdddr(car(xs)); });

    define<procedure>("caaaar", [](let const& xs) { return caaaar(car(xs)); });
    define<procedure>("caaadr", [](let const& xs) { return caaadr(car(xs)); });
    define<procedure>("caadar", [](let const& xs) { return caadar(car(xs)); });
    define<procedure>("caaddr", [](let const& xs) { return caaddr(car(xs)); });
    define<procedure>("cadaar", [](let const& xs) { return cadaar(car(xs)); });
    define<procedure>("cadadr", [](let const& xs) { return cadadr(car(xs)); });
    define<procedure>("caddar", [](let const& xs) { return caddar(car(xs)); });
    define<procedure>("cadddr", [](let const& xs) { return cadddr(car(xs)); });
    define<procedure>("cdaaar", [](let const& xs) { return cdaaar(car(xs)); });
    define<procedure>("cdaadr", [](let const& xs) { return cdaadr(car(xs)); });
    define<procedure>("cdadar", [](let const& xs) { return cdadar(car(xs)); });
    define<procedure>("cdaddr", [](let const& xs) { return cdaddr(car(xs)); });
    define<procedure>("cddaar", [](let const& xs) { return cddaar(car(xs)); });
    define<procedure>("cddadr", [](let const& xs) { return cddadr(car(xs)); });
    define<procedure>("cdddar", [](let const& xs) { return cdddar(car(xs)); });
    define<procedure>("cddddr", [](let const& xs) { return cddddr(car(xs)); });

    define<procedure>("set-car!", [](auto&& xs) { return caar(xs) = cadr(xs); });
    define<procedure>("set-cdr!", [](auto&& xs) { return cdar(xs) = cadr(xs); });

    export_("pair?");
    export_("cons");
    export_("car");
    export_("cdr");
    export_("set-car!");
    export_("set-cdr!");
    export_("caaaar");
    export_("caaadr");
    export_("caaar");
    export_("caadar");
    export_("caaddr");
    export_("caadr");
    export_("caar");
    export_("cadaar");
    export_("cadadr");
    export_("cadar");
    export_("caddar");
    export_("cadddr");
    export_("caddr");
    export_("cadr");
    export_("cdaaar");
    export_("cdaadr");
    export_("cdaar");
    export_("cdadar");
    export_("cdaddr");
    export_("cdadr");
    export_("cdar");
    export_("cddaar");
    export_("cddadr");
    export_("cddar");
    export_("cdddar");
    export_("cddddr");
    export_("cdddr");
    export_("cddr");
  }

  library::library(list_library_t)
  {
    define<predicate>("null?", [](let const& xs)
    {
      return car(xs).is<null>();
    });

    define<procedure>("append", [](let const& xs)
    {
      return std::accumulate(std::begin(xs), std::end(xs), unit, append2);
    });

    define<procedure>("list->string", [](let const& xs)
    {
      string s;

      for (let const& x : car(xs))
      {
        s.push_back(x.as<character>());
      }

      return make(std::move(s));
    });

    define<procedure>("list->vector", [](let const& xs)
    {
      return make<vector>(car(xs));
    });

    export_("null?");
    export_("append");
    export_("list->string");
    export_("list->vector");
  }

  library::library(symbol_library_t)
  {
    define<predicate>("symbol?", [](let const& xs)
    {
      return car(xs).is<symbol>();
    });

    define<procedure>("symbol->string", [](let const& xs)
    {
      return make<string>(car(xs).as<symbol>());
    });

    export_("symbol?");
    export_("symbol->string");
  }

  library::library(character_library_t)
  {
    define<predicate>("char?", [](let const& xs)
    {
      return car(xs).is<character>();
    });

    define<procedure>("char->integer", [](let const& xs)
    {
      if (xs.is<pair>() and car(xs).is<character>())
      {
        return make<exact_integer>(car(xs).as<character>().codepoint);
      }
      else
      {
        throw invalid_application(intern("char->integer") | xs);
      }
    });

    define<procedure>("char-codepoint", [](let const& xs) -> value_type
    {
      if (auto c = car(xs).as<character>(); std::isdigit(c.codepoint))
      {
        return make<exact_integer>(c.codepoint - '0');
      }
      else
      {
        return f;
      }
    });

    export_("char?");
    export_("char->integer");
    export_("char-codepoint");
  }

  library::library(string_library_t)
  {
    define<predicate>("string?", [](let const& xs) { return car(xs).is<string>(); });

    define<procedure>("make-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return make<string>(static_cast<string::size_type>(car(xs).as<exact_integer>()), character());

      case 2:
        return make<string>(static_cast<string::size_type>(car(xs).as<exact_integer>()), cadr(xs).as<character>());

      default:
        throw invalid_application(intern("make-string") | xs);
      }
    });

    define<procedure>("string-length", [](let const& xs)
    {
      return make<exact_integer>(car(xs).as<string>().size());
    });

    define<procedure>("string-ref", [](let const& xs)
    {
      return make(car(xs).as<string>().at(static_cast<string::size_type>(cadr(xs).as<exact_integer>())));
    });

    define<procedure>("string-set!", [](let const& xs)
    {
      car(xs).as<string>().at(static_cast<string::size_type>(cadr(xs).as<exact_integer>())) = caddr(xs).as<character>();
      return car(xs);
    });

    define<procedure>("string-append", [](let const& xs)
    {
      string result;

      for (let const& x : xs)
      {
        std::copy(std::cbegin(x.as<string>()), std::cend(x.as<string>()), std::back_inserter(result));
      }

      return make(result);
    });

    define<procedure>("string-copy", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return make<string>(car(xs).as<string>());

      case 2:
        return make<string>(car(xs).as<string>().begin() + static_cast<string::size_type>(cadr(xs).as<exact_integer>()),
                            car(xs).as<string>().end());
      case 3:
        return make<string>(car(xs).as<string>().begin() + static_cast<string::size_type>( cadr(xs).as<exact_integer>()),
                            car(xs).as<string>().begin() + static_cast<string::size_type>(caddr(xs).as<exact_integer>()));
      default:
        throw invalid_application(intern("string-copy") | xs);
      }
    });

    #define STRING_COMPARE(COMPARE)                                            \
    [](let const& xs)                                                          \
    {                                                                          \
      return std::adjacent_find(                                               \
               std::begin(xs), std::end(xs), [](let const& a, let const& b)    \
               {                                                               \
                 return not COMPARE(a.as_const<string>(),                      \
                                    b.as_const<string>());                     \
               }) == std::end(xs);                                             \
    }

    define<predicate>("string=?",  STRING_COMPARE(std::equal_to     <void>()));
    define<predicate>("string<?",  STRING_COMPARE(std::less         <void>()));
    define<predicate>("string<=?", STRING_COMPARE(std::less_equal   <void>()));
    define<predicate>("string>?",  STRING_COMPARE(std::greater      <void>()));
    define<predicate>("string>=?", STRING_COMPARE(std::greater_equal<void>()));

    #undef STRING_COMPARE

    define<procedure>("string->number", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return string_to::number(car(xs).as<string>(), 10);

      case 2:
        return string_to::number(car(xs).as<string>(), static_cast<int>(cadr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("string->number") | xs);
      }
    });

    define<procedure>("string->list", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<string>().list();

      case 2:
        return car(xs).as<string>().list(static_cast<string::size_type>(cadr(xs).as<exact_integer>()));

      case 3:
        return car(xs).as<string>().list(static_cast<string::size_type>(cadr(xs).as<exact_integer>()), static_cast<string::size_type>(caddr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("string->list") | xs);
      }
    });

    define<procedure>("string->symbol", [](let const& xs)
    {
      return intern(car(xs).as<string>());
    });

    export_("string?");
    export_("make-string");
    export_("string-append");
    export_("string-copy");
    export_("string-length");
    export_("string-ref");
    export_("string-set!");
    export_("string=?");
    export_("string<?");
    export_("string<=?");
    export_("string>?");
    export_("string>=?");
    export_("string->list");
    export_("string->number");
    export_("string->symbol");
  }

  library::library(vector_library_t)
  {
    define<predicate>("vector?", [](let const& xs)
    {
      return car(xs).is<vector>();
    });

    define<procedure>("vector", [](let const& xs)
    {
      return make<vector>(xs);
    });

    define<procedure>("make-vector", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return make<vector>(car(xs), unspecified);

      case 2:
        return make<vector>(car(xs), cadr(xs));

      default:
        throw invalid_application(intern("make-vector") | xs);
      }
    });

    define<procedure>("vector-length", [](let const& xs)
    {
      return car(xs).as<vector>().length();
    });

    define<procedure>("vector-ref", [](let const& xs)
    {
      return car(xs).as<vector>().ref(cadr(xs));
    });

    define<procedure>("vector-set!", [](let const& xs)
    {
      return car(xs).as<vector>().set(cadr(xs), caddr(xs));
    });

    define<procedure>("vector-fill!", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        car(xs).as<vector>().fill(cadr(xs), e0, car(xs).as<vector>().length());
        break;

      case 3:
        car(xs).as<vector>().fill(cadr(xs), caddr(xs), car(xs).as<vector>().length());
        break;

      case 4:
        car(xs).as<vector>().fill(cadr(xs), caddr(xs), cadddr(xs));
        break;

      default:
        throw invalid_application(intern("vector-fill!") | xs);
      }

      return unspecified;
    });

    define<procedure>("vector->list", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<vector>().list(e0, car(xs).as<vector>().length());

      case 2:
        return car(xs).as<vector>().list(cadr(xs), car(xs).as<vector>().length());

      case 3:
        return car(xs).as<vector>().list(cadr(xs), caddr(xs));

      default:
        throw invalid_application(intern("vector->list") | xs);
      }
    });

    define<procedure>("vector->string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<vector>().string(e0, car(xs).as<vector>().length());

      case 2:
        return car(xs).as<vector>().string(cadr(xs), car(xs).as<vector>().length());

      case 3:
        return car(xs).as<vector>().string(cadr(xs), caddr(xs));

      default:
        throw invalid_application(intern("vector->string") | xs);
      }
    });

    export_("vector?");
    export_("vector");
    export_("make-vector");
    export_("vector-length");
    export_("vector-ref");
    export_("vector-set!");
    export_("vector-fill!");
    export_("vector->list");
    export_("vector->string");
  }

  library::library(control_library_t)
  {
    define<predicate>("closure?", [](let const& xs)
    {
      return car(xs).is<closure>();
    });

    define<predicate>("continuation?", [](let const& xs)
    {
      return car(xs).is<continuation>();
    });

    export_("closure?");
    export_("continuation?");
  }

  library::library(exception_library_t)
  {
    define<procedure>("throw", [](let const& xs) -> value_type
    {
      throw car(xs);
    });

    define<procedure>("make-error", [](let const& xs)
    {
      return make<error>(car(xs), cdr(xs));
    });

    define<predicate>("error?", [](let const& xs)
    {
      return car(xs).is<error>();
    });

    define<predicate>("read-error?", [](let const& xs)
    {
      return car(xs).is<read_error>();
    });

    define<predicate>("file-error?", [](let const& xs)
    {
      return car(xs).is<file_error>();
    });

    define<predicate>("syntax-error?", [](let const& xs)
    {
      return car(xs).is<syntax_error>();
    });

    export_("throw");
    export_("make-error");
    export_("error?");
    export_("read-error?");
    export_("file-error?");
    export_("syntax-error?");
  }

  library::library(port_library_t)
  {
    define<predicate>(  "input-port?", [](let const& xs) { return car(xs).is_also<std::istream>(); });
    define<predicate>( "output-port?", [](let const& xs) { return car(xs).is_also<std::ostream>(); });
    define<predicate>( "binary-port?", [](let const&   ) { return false;                           });
    define<predicate>("textual-port?", [](let const& xs) { return car(xs).is_also<std::ios    >(); });
    define<predicate>(        "port?", [](let const& xs) { return car(xs).is_also<std::ios    >(); });

    define<predicate>("input-port-open?", [](let const& xs)
    {
      if (let const& x = car(xs); x.is_also<std::ifstream>())
      {
        return x.as<std::ifstream>().is_open();
      }
      else
      {
        return x.is_also<std::istream>();
      }
    });

    define<predicate>("output-port-open?", [](let const& xs)
    {
      if (let const& x = car(xs); x.is_also<std::ofstream>())
      {
        return x.as<std::ofstream>().is_open();
      }
      else
      {
        return x.is_also<std::ostream>();
      }
    });

    define<procedure>("standard-input-port",  [](auto&&...) { return standard_input;  });
    define<procedure>("standard-output-port", [](auto&&...) { return standard_output; });
    define<procedure>("standard-error-port",  [](auto&&...) { return standard_error;  });

    define<procedure>("open-input-file",  [](let const& xs) { return make< input_file_port>(car(xs).as<string>()); });
    define<procedure>("open-output-file", [](let const& xs) { return make<output_file_port>(car(xs).as<string>()); });

    define<procedure>("close-input-port", [](let const& xs)
    {
      if (let const& x = car(xs); x.is_also<std::ifstream>())
      {
        x.as<std::ifstream>().close();
      }

      return unspecified;
    });

    define<procedure>("close-output-port", [](let const& xs)
    {
      if (let const& x = car(xs); x.is_also<std::ofstream>())
      {
        x.as<std::ofstream>().close();
      }

      return unspecified;
    });

    define<procedure>("open-input-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 0:
        return make<input_string_port>();

      case 1:
        return make<input_string_port>(car(xs).as<string>());

      default:
        throw invalid_application(intern("open-input-string") | xs);
      }
    });

    define<procedure>("open-output-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 0:
        return make<output_string_port>();

      case 1:
        return make<output_string_port>(car(xs).as<string>());

      default:
        throw invalid_application(intern("open-output-string") | xs);
      }
    });

    define<procedure>("get-output-string", [](let const& xs)
    {
      return make<string>(car(xs).as<std::ostringstream>().str());
    });

    define<procedure>("%read-char", [](let const& xs) -> value_type
    {
      try
      {
        return make<character>(car(xs).as<std::istream>());
      }
      catch (eof const&)
      {
        return eof_object;
      }
      catch (read_error const& error)
      {
        return make(error);
      }
    });

    define<procedure>("%peek-char", [](let const& xs) -> value_type
    {
      try
      {
        auto const g = car(xs).as<std::istream>().tellg();
        let const c = make<character>(car(xs).as<std::istream>());
        car(xs).as<std::istream>().seekg(g);
        return c;
      }
      catch (eof const&)
      {
        return eof_object;
      }
      catch (read_error const& error)
      {
        return make(error);
      }
    });

    define<predicate>("eof-object?", [](let const& xs)
    {
      return car(xs).is<eof>();
    });

    define<procedure>("eof-object", [](auto&&...)
    {
      return eof_object;
    });

    define<predicate>("read-ready?", [](let const& xs)
    {
      return static_cast<bool>(car(xs).as<std::istream>());
    });

    define<procedure>("%read-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        return make<string>(cadr(xs).as<std::istream>(), static_cast<string::size_type>(car(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("read-string") | xs);
      }
    });

    define<procedure>("put-char", [](let const& xs)
    {
      cadr(xs).as<std::ostream>() << static_cast<external_representation>(car(xs).as<character>());
      return unspecified;
    });

    define<procedure>("put-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        cadr(xs).as<std::ostream>() << static_cast<external_representation>(car(xs).as<string>());
        break;

      case 3: // TODO
      case 4: // TODO

      default:
        throw invalid_application(intern("write-string") | xs);
      }

      return unspecified;
    });

    define<procedure>("%flush-output-port", [](let const& xs)
    {
      car(xs).as<std::ostream>() << std::flush;
      return unspecified;
    });

    export_("input-port?");
    export_("output-port?");
    export_("binary-port?");
    export_("textual-port?");
    export_("port?");
    export_("input-port-open?");
    export_("output-port-open?");
    export_("standard-input-port");
    export_("standard-output-port");
    export_("standard-error-port");
    export_("open-input-file");
    export_("open-output-file");
    export_("close-input-port");
    export_("close-output-port");
    export_("open-input-string");
    export_("open-output-string");
    export_("get-output-string");
    export_("%read-char");
    export_("%peek-char");
    export_("eof-object?");
    export_("eof-object");
    export_("read-ready?");
    export_("%read-string");
    export_("put-char");
    export_("put-string");
    export_("%flush-output-port");
  }

  library::library(evaluate_library_t)
  {
    define<procedure>("eval", [](let const& xs)
    {
      return cadr(xs).as<environment>().evaluate(car(xs));
    });

    export_("eval");
  }

  library::library(read_library_t)
  {
    define<procedure>("%read", [this](let const& xs) -> value_type
    {
      try
      {
        return read(car(xs));
      }
      catch (eof const&)
      {
        return eof_object;
      }
      catch (read_error const& error)
      {
        return make(error);
      }
    });

    export_("%read");
  }

  library::library(write_library_t)
  {
    define<procedure>("%write-simple", [](let const& xs)
    {
      kernel::write(cadr(xs), car(xs));
      return unspecified;
    });

    define<procedure>("print", [](let const& xs)
    {
      for (let const& x : xs)
      {
        if (x.is<string>())
        {
          std::cout << static_cast<external_representation>(x.as<string>());
        }
        else
        {
          std::cout << x;
        }
      }

      std::cout << std::endl;

      return standard_output;
    });

    export_("%write-simple");
    export_("print");
  }

  library::library(macro_library_t)
  {
    define<predicate>("identifier?", [](let const& xs)
    {
      return car(xs).is_also<identifier>();
    });

    define<procedure>("identifier->symbol", [](let const& xs)
    {
      if (let const& x = car(xs); x.is<syntactic_closure>())
      {
        return x.as<syntactic_closure>().expression;
      }
      else
      {
        return x;
      }
    });

    define<predicate>("transformer?", [](let const& xs)
    {
      return car(xs).is_also<transformer>();
    });

    define<predicate>("syntactic-closure?", [](let const& xs)
    {
      return car(xs).is<syntactic_closure>();
    });

    define<procedure>("make-syntactic-closure", [](let const& xs)
    {
      return make<syntactic_closure>(car(xs), cadr(xs), caddr(xs));
    });

    export_("identifier?");
    export_("identifier->symbol");
    export_("transformer?");
    export_("syntactic-closure?");
    export_("make-syntactic-closure");
  }

  library::library(experimental_library_t)
  {
    define<procedure>("type-of", [](let const& xs)
    {
      std::cout << car(xs).type().name() << std::endl;

      return standard_output;
    });

    define<procedure>("disassemble", [](let const& xs)
    {
      if (0 < length(xs))
      {
        if (let const& f = car(xs); f.is<closure>())
        {
          disassemble(std::cout, car(f));
        }
      }

      return standard_output;
    });

    define<predicate>("ieee-float?", [](auto&&...)
    {
      return std::numeric_limits<double>::is_iec559;
    });

    export_("type-of");
    export_("disassemble");
    export_("ieee-float?");
  }

  library::library(context_library_t)
  {
    define<procedure>("emergency-exit", [](let const& xs) -> value_type
    {
      switch (length(xs))
      {
      case 0:
        throw exit_status::success;

      case 1:
        if (let const& x = car(xs); x.is<bool>())
        {
          throw select(x) ? exit_status::success : exit_status::failure;
        }
        else if (x.is<exact_integer>())
        {
          throw exit_status(static_cast<int>(x.as<exact_integer>()));
        }
        else [[fallthrough]];

      default:
        throw invalid_application(intern("emergency-exit") | xs);
      }
    });

    export_("emergency-exit");
  }

  library::library(const_reference declarations)
    : declarations { declarations }
  {
    build();
  }

  auto library::boot() -> void
  {
    define_library("(meevax character)", character_library);
    define_library("(meevax context)", context_library);
    define_library("(meevax control)", control_library);
    define_library("(meevax environment)", environment_library);
    define_library("(meevax equivalence)", equivalence_library);
    define_library("(meevax evaluate)", evaluate_library);
    define_library("(meevax exception)", exception_library);
    define_library("(meevax experimental)", experimental_library);
    define_library("(meevax inexact)", inexact_library);
    define_library("(meevax list)", list_library);
    define_library("(meevax macro)", macro_library);
    define_library("(meevax number)", number_library);
    define_library("(meevax pair)", pair_library);
    define_library("(meevax port)", port_library);
    define_library("(meevax read)", read_library);
    define_library("(meevax string)", string_library);
    define_library("(meevax symbol)", symbol_library);
    define_library("(meevax syntax)", syntax_library);
    define_library("(meevax vector)", vector_library);
    define_library("(meevax write)", write_library);

    define_library("(meevax foreign-function)", [](library & library)
    {
      library.define<procedure>("foreign-function", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<string>(), car(xs).as<string>());
      });

      library.define<predicate>("foreign-function?", [](let const& xs)
      {
        return car(xs).is<procedure>();
      });

      library.export_("foreign-function");
      library.export_("foreign-function?");
    });

    define_library("(meevax garbage-collector)", [](library & library)
    {
      library.define<procedure>("gc-collect", [](auto&&...)
      {
        return make<exact_integer>(gc.collect());
      });

      library.define<procedure>("gc-count", [](auto&&...)
      {
        return make<exact_integer>(gc.count());
      });

      library.export_("gc-collect");
      library.export_("gc-count");
    });

    define_library("(meevax version)", [](library & library)
    {
      library.define<procedure>("features", [](auto&&...)
      {
        return features();
      });

      library.export_("features");
    });

    std::vector<string_view> const codes {
      srfi_211,
      r4rs_essential,
      srfi_45,
      r4rs,
      srfi_149,
      r5rs,
      srfi_6,  // Basic String Ports
      srfi_34, // Exception Handling for Programs
      srfi_23, // Error reporting mechanism
      srfi_39, // Parameter objects
      r7rs,
      srfi_8,  // receive: Binding to multiple values
      srfi_1,  // List Library
      srfi_78, // Lightweight testing
    };

    auto sandbox = environment();

    for (auto const& code : codes)
    {
      // NOTE: Since read performs a putback operation on a given stream, it must be copied and used.
      auto port = std::stringstream(std::string(code));

      for (let e = sandbox.read(port); e != eof_object; e = sandbox.read(port))
      {
        sandbox.evaluate(e);
      }
    }
  }

  auto library::build() -> void
  {
    if (export_specs.is<null>())
    {
      for (let const& declaration : declarations)
      {
        evaluate(declaration);
      }
    }
  }

  auto library::evaluate(const_reference declaration) -> void
  {
    if (declaration.is<pair>() and car(declaration).is<symbol>()
                               and car(declaration).as<symbol>().value == "export")
    {
      for (let const& export_spec : cdr(declaration))
      {
        export_(export_spec);
      }
    }
    else if (declaration.is<pair>() and car(declaration).is<symbol>()
                                    and car(declaration).as<symbol>().value == "begin")
    {
      for (let const& command_or_definition : cdr(declaration))
      {
        evaluate(command_or_definition);
      }
    }
    else
    {
      environment::evaluate(declaration); // Non-standard extension.
    }
  }

  auto library::export_(const_reference export_spec) -> void
  {
    export_specs = cons(export_spec, export_specs);
  }

  auto library::export_(external_representation const& export_spec) -> void
  {
    export_(read(export_spec));
  }

  auto library::resolve_export_specs() -> value_type
  {
    return map([this](let const& export_spec)
    {
      if (export_spec.is<pair>() and car(export_spec).is<symbol>()
                                 and car(export_spec).as<symbol>().value == "rename")
      {
        if (let const& binding = identify(cadr(export_spec), unit); binding.as<identity>().is_free())
        {
          throw error(make<string>("exported but undefined"), cadr(export_spec));
        }
        else
        {
          return make<absolute>(caddr(export_spec), binding.as<absolute>().load());
        }
      }
      else
      {
        if (let const& binding = identify(export_spec, unit); binding.as<identity>().is_free())
        {
          throw error(make<string>("exported but undefined"), export_spec);
        }
        else
        {
          return binding;
        }
      }
    }, export_specs);
  }

  auto operator <<(std::ostream & os, library const& library) -> std::ostream &
  {
    return os << library.global();
  }

  std::unordered_map<external_representation, library> libraries {};
} // namespace kernel
} // namespace meevax
