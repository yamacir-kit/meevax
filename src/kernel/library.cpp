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

    export_("begin",
            "call-with-current-continuation!",
            "define",
            "define-syntax",
            "if",
            "lambda",
            "let-syntax",
            "letrec",
            "letrec-syntax",
            "quote",
            "quote-syntax",
            "set!");
  }

  library::library(equivalence_library_t)
  {
    define<predicate>("eq?",  [](let const& xs) { return eq (car(xs), cadr(xs)); });
    define<predicate>("eqv?", [](let const& xs) { return eqv(car(xs), cadr(xs)); });

    export_("eq?", "eqv?");
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

    define<procedure>("+", [](let const& xs) { return std::accumulate(std::begin(xs), std::end(xs), e0, [](let const& a, let const& b) { return a + b; }); });
    define<procedure>("*", [](let const& xs) { return std::accumulate(std::begin(xs), std::end(xs), e1, [](let const& a, let const& b) { return a * b; }); });

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
      return make<string>(lexical_cast<std::string>(car(xs)));
    });

    export_("number?", "complex?", "real?", "rational?", "integer?",
            "exact-integer?", "%complex?", "ratio?", "single-float?", "double-float?",

            "=", "!=", "<", "<=", ">", ">=", "+", "*", "-", "/", "%",

            "floor", "ceiling", "truncate", "round",

            "expt",

            "exact", "inexact",

            "integer->char", "number->string");
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

    export_("finite?", "infinite?", "nan?",
            "exp", "sqrt", "log",
            "sin", "asin", "sinh", "asinh",
            "cos", "acos", "cosh", "acosh",
            "tan", "atan", "tanh", "atanh");
  }

  library::library(pair_library_t)
  {
    define<predicate>("pair?", [](let const& xs)
    {
      return car(xs).is<pair>();
    });

    define<instruction_level_procedure>("cons", cons_, [](let const& xs)
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

    export_("pair?", "cons",

            "car", "cdr",

            "caar", "cadr", "cdar", "cddr",

            "caaar", "caadr", "cadar", "caddr",
            "cdaar", "cdadr", "cddar", "cdddr",

            "caaaar", "caaadr", "caadar", "caaddr",
            "cadaar", "cadadr", "caddar", "cadddr",
            "cdaaar", "cdaadr", "cdadar", "cdaddr",
            "cddaar", "cddadr", "cdddar", "cddddr",

            "set-car!", "set-cdr!");
  }

  library::library(list_library_t)
  {
    define<predicate>("null?", [](let const& xs)
    {
      return car(xs).is<null>();
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
      return make<vector>(for_each_in, car(xs));
    });

    export_("null?",
            "list->string",
            "list->vector");
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

    export_("symbol?", "symbol->string");
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

    define<procedure>("digit-value", [](let const& xs)
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

    export_("char?",
            "char->integer",
            "digit-value" // TODO => character:codepoint
            );
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

    export_("string?", "make-string",

            "string-append",
            "string-copy",
            "string-length",
            "string-ref",
            "string-set!",

            "string=?",
            "string<?",
            "string<=?",
            "string>?",
            "string>=?",

            "string->list",
            "string->number",
            "string->symbol"
            );
  }

  library::library(vector_library_t)
  {
    define<predicate>("vector?", [](let const& xs)
    {
      return car(xs).is<vector>();
    });

    define<procedure>("vector", [](let const& xs)
    {
      return make<vector>(for_each_in, xs);
    });

    define<procedure>("make-vector", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return make<vector>(static_cast<vector::size_type>(car(xs).as<exact_integer>()), unspecified_object);

      case 2:
        return make<vector>(static_cast<vector::size_type>(car(xs).as<exact_integer>()), cadr(xs));

      default:
        throw invalid_application(intern("make-vector") | xs);
      }
    });

    define<procedure>("vector-length", [](let const& xs)
    {
      return make<exact_integer>(car(xs).as<vector>().size());
    });

    define<procedure>("vector-ref", [](let const& xs)
    {
      return car(xs).as<vector>().at(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()));
    });

    define<procedure>("vector-set!", [](let const& xs)
    {
      return car(xs).as<vector>().at(static_cast<vector::size_type>(cadr(xs).as<exact_integer>())) = caddr(xs);
    });

    define<procedure>("vector-fill!", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        car(xs).as<vector>().fill(cadr(xs));
        break;

      case 3:
        car(xs).as<vector>().fill(cadr(xs), static_cast<string::size_type>(caddr(xs).as<exact_integer>()));
        break;

      case 4:
        car(xs).as<vector>().fill(cadr(xs), static_cast<string::size_type>(caddr(xs).as<exact_integer>()), static_cast<string::size_type>(cadddr(xs).as<exact_integer>()));
        break;

      default:
        throw invalid_application(intern("vector-fill!") | xs);
      }

      return unspecified_object;
    });

    define<procedure>("vector->list", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<vector>().list();

      case 2:
        return car(xs).as<vector>().list(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()));

      case 3:
        return car(xs).as<vector>().list(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()), static_cast<vector::size_type>(caddr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("vector->list") | xs);
      }
    });

    define<procedure>("vector->string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<vector>().string();

      case 2:
        return car(xs).as<vector>().string(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()));

      case 3:
        return car(xs).as<vector>().string(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()), static_cast<vector::size_type>(caddr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("vector->string") | xs);
      }
    });

    export_("vector?",

            "vector", "make-vector",

            "vector-length",
            "vector-ref",
            "vector-set!",
            "vector-fill!",

            "vector->list",
            "vector->string");
  }

  library::library(control_library_t)
  {
    define<predicate>("closure?",          [](let const& xs) { return car(xs).is<closure     >(); });
    define<predicate>("continuation?",     [](let const& xs) { return car(xs).is<continuation>(); });
    define<predicate>("foreign-function?", [](let const& xs) { return car(xs).is<procedure   >(); });

    export_("closure?",
            "continuation?",
            "foreign-function?");
  }

  library::library(exception_library_t)
  {
    define<procedure>("default-exception-handler", [](let const& xs) -> object
    {
      throw car(xs);
    });

    define<procedure>("make-error", [](let const& xs)
    {
      return make<error>(car(xs), cdr(xs));
    });

    define<predicate>(       "error?", [](let const& xs) { return car(xs).is<       error>(); });
    define<predicate>(  "read-error?", [](let const& xs) { return car(xs).is<  read_error>(); });
    define<predicate>(  "file-error?", [](let const& xs) { return car(xs).is<  file_error>(); });
    define<predicate>("syntax-error?", [](let const& xs) { return car(xs).is<syntax_error>(); });

    export_("default-exception-handler",
            "make-error",
            "error?",
            "read-error?",
            "file-error?",
            "syntax-error?");
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

      return unspecified_object;
    });

    define<procedure>("close-output-port", [](let const& xs)
    {
      if (let const& x = car(xs); x.is_also<std::ofstream>())
      {
        x.as<std::ofstream>().close();
      }

      return unspecified_object;
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

    define<procedure>("%read-char", [](let const& xs)
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

    define<procedure>("%peek-char", [](let const& xs)
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

    define<predicate>("%char-ready?", [](let const& xs)
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
      cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<character>());
      return unspecified_object;
    });

    define<procedure>("put-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<string>());
        break;

      case 3: // TODO
      case 4: // TODO

      default:
        throw invalid_application(intern("write-string") | xs);
      }

      return unspecified_object;
    });

    define<procedure>("%flush-output-port", [](let const& xs)
    {
      car(xs).as<std::ostream>() << std::flush;
      return unspecified_object;
    });

    export_("input-port?",
            "output-port?",
            "binary-port?",
            "textual-port?",
            "port?",
            "input-port-open?",
            "output-port-open?",
            "standard-input-port",
            "standard-output-port",
            "standard-error-port",
            "open-input-file",
            "open-output-file",
            "close-input-port",
            "close-output-port",
            "open-input-string",
            "open-output-string",
            "get-output-string",
            "%read-char",
            "%peek-char",
            "eof-object?",
            "eof-object",
            "%char-ready?",
            "%read-string",
            "put-char",
            "put-string",
            "%flush-output-port");
  }

  library::library(evaluate_library_t)
  {
    define<procedure>("eval", [](let const& xs)
    {
      return cadr(xs).as<transformer>().mac_env.as<environment>().evaluate(car(xs)); // DIRTY HACK!
    });

    export_("eval");
  }

  library::library(read_library_t)
  {
    define<procedure>("%read", [this](let const& xs)
    {
      try
      {
        switch (length(xs))
        {
        case 0:
          return read(standard_input);

        case 1:
          return read(car(xs));

        default:
          throw invalid_application(intern("read") | xs);
        }
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
    define<procedure>("%write-simple", [this](let const& xs)
    {
      write(cadr(xs), car(xs));
      return unspecified_object;
    });

    define<procedure>("print", [](let const& xs)
    {
      for (let const& x : xs)
      {
        if (x.is<string>())
        {
          std::cout << static_cast<std::string>(x.as<string>());
        }
        else
        {
          std::cout << x;
        }
      }

      std::cout << std::endl;

      return standard_output;
    });

    export_("%write-simple", "print");
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

    export_("identifier?",
            "identifier->symbol",
            "transformer?",
            "syntactic-closure?",
            "make-syntactic-closure");
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

    export_("type-of",
            "disassemble",
            "ieee-float?"
            );
  }

  library::library(context_library_t)
  {
    define<procedure>("emergency-exit", [](let const& xs) -> object
    {
      switch (length(xs))
      {
      case 0:
        throw exit_status::success;

      case 1:
        if (let const& x = car(xs); x.is<boolean>())
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

  std::map<std::string, library> libraries {};

  auto library::boot() -> void
  {
    define_library("(meevax character)", character_library);
    define_library("(meevax context)", context_library);
    define_library("(meevax control)", control_library);
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

    define_library("(meevax gc)", [](library & meevax_gc)
    {
      meevax_gc.define<procedure>("gc-collect", [](auto&&...)
      {
        return make<exact_integer>(gc.collect());
      });

      meevax_gc.define<procedure>("gc-count", [](auto&&...)
      {
        return make<exact_integer>(gc.count());
      });

      meevax_gc.export_("gc-collect", "gc-count");
    });

    define_library("(meevax foreign-function-interface)", [](library & meevax_ffi)
    {
      meevax_ffi.define<procedure>("foreign-function", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<string>(), car(xs).as<string>());
      });

      meevax_ffi.export_("foreign-function");
    });

    define_library("(meevax foo)", [](library & library)
    {
      library.export_("a");
      library.export_("b");
      library.export_("c");

      library.define<procedure>("a", [](let const&)
      {
        LINE();
        return unit;
      });

      library.define("b", make<exact_integer>(42));

      library.define("c", make<symbol>("dummy"));
    });
  }
} // namespace kernel
} // namespace meevax
