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

#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/syntax.hpp>
#include <meevax/kernel/version.hpp>
#include <meevax/kernel/writer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  class configurator
  {
    friend Environment;

    explicit configurator()
    {}

  public:
    static inline auto batch       = false;
    static inline auto debug       = false;
    static inline auto interactive = true;
    static inline auto trace       = false;

    static auto display_help() -> void
    {
      print("Meevax Lisp ", version());
      print();
      print("Usage: meevax [OPTION...] [FILE...]");
      print();
      print("Options:");
      print("  -b, --batch            Suppress any system output.");
      print("  -d, --debug            Deprecated.");
      print("  -e, --evaluate=STRING  Read and evaluate given STRING at configuration step.");
      print("  -h, --help             Display this help text and exit.");
      print("  -i, --interactive      Take over control of root environment.");
      print("  -l, --load=FILENAME    Same as -e '(load FILENAME)'");
      print("  -t, --trace            Display stacks of virtual machine for each steps.");
      print("  -v, --version          Display version information and exit.");
      print("  -w, --write=OBJECT     Same as -e '(write OBJECT)'");
    }

    struct option
    {
      let const operation;

      bool const requires_an_argument;

      template <typename S, typename F, std::enable_if_t<std::is_invocable_v<F>, std::nullptr_t> = nullptr>
      explicit option(S&& s, F&& f)
        : operation {
            make<procedure>(std::forward<decltype(s)>(s), [f](auto&&...)
            {
              f();
              return unit;
            })
          }
        , requires_an_argument { false }
      {}

      template <typename S, typename F, std::enable_if_t<std::is_invocable_v<F, let const&>, std::nullptr_t> = nullptr>
      explicit option(S&& s, F&& f)
        : operation { make<procedure>(std::forward<decltype(s)>(s),
                                      std::forward<decltype(f)>(f)) }
        , requires_an_argument { true }
      {}

      template <typename... Ts>
      auto match(Ts&&... xs) const
      {
        return std::regex_match(std::forward<decltype(xs)>(xs)..., std::regex(operation.as<procedure>().name));
      }
    };

    auto configure(const int argc, char const* const* const argv)
    {
      return configure({ argv + 1, argv + argc });
    }

    auto configure(std::vector<std::string> const& args) -> void
    {
      static std::regex const pattern { R"(--(\w[-\w]+)(=(.*))?|-([\w]+))" };

      auto read = [](auto&&... xs)
      {
        return interaction_environment().as<Environment>().read(std::forward<decltype(xs)>(xs)...);
      };

      std::vector<option> options
      {
        option("(b|batch)",       [this]() { batch       = true; }),
        option("(d|debug)",       [this]() { debug       = true; }),
        option("(i|interactive)", [this]() { interactive = true; }),
        option("(t|trace)",       [this]() { trace       = true; }),

        option("(e|evaluate)", [this](let const& xs)
        {
          return static_cast<Environment &>(*this).evaluate(xs[0]);
        }),

        option("(h|help)", []()
        {
          display_help();
          throw success;
        }),

        option("(l|load)", [this](let const& xs)
        {
          static_cast<Environment &>(*this).load(xs[0].as<string>());
          return unit;
        }),

        option("(v|version)", []()
        {
          std::cout << version() << std::endl;
          throw success;
        }),

        option("(w|write)", [](let const& xs)
        {
          std::cout << xs[0] << std::endl;
          return unit;
        }),
      };

      auto search = [&](auto&& name) -> auto const&
      {
        if (auto iter = std::find_if(std::begin(options), std::end(options), [&](auto&& option)
                        {
                          return option.match(name);
                        });
            iter != std::end(options))
        {
          return *iter;
        }
        else
        {
          throw error(make<string>("unknown option"), make<symbol>(name));
        }
      };

      std::vector<object> expressions {};

      let const quote = make<syntax>("quote", Environment::quote);

      for (auto iter = std::begin(args); iter != std::end(args); ++iter)
      {
        static std::regex const pattern { R"(--(\w[-\w]+)(?:=(.*))?|-([\w]+))" };

        auto read_argument = [&]()
        {
          if (std::next(iter) != std::cend(args))
          {
            return read(*++iter);
          }
          else
          {
            throw error(make<string>("an argument required but not specified"),
                        make<symbol>(*iter));
          }
        };

        if (std::smatch result; std::regex_match(*iter, result, pattern))
        {
          if (result.length(3))
          {
            for (auto str3 = result.str(3); not str3.empty(); str3.erase(0, 1))
            {
              if (auto&& option = search(str3.substr(0, 1)); option.requires_an_argument)
              {
                expressions.push_back(list(option.operation, list(quote, 1 < str3.length() ? read(str3.substr(1)) : read_argument())));
                break;
              }
              else
              {
                expressions.push_back(list(option.operation));
              }
            }
          }
          else if (result.length(2))
          {
            expressions.push_back(list(search(result.str(1)).operation, list(quote, read(result.str(2)))));
          }
          else if (result.length(1))
          {
            if (auto&& option = search(result.str(1)); option.requires_an_argument)
            {
              expressions.push_back(list(option.operation, list(quote, read_argument())));
            }
            else
            {
              expressions.push_back(list(option.operation));
            }
          }
        }
        else
        {
          expressions.push_back(list(search("load").operation, make<string>(*iter)));
          interactive = false;
        }
      }

      for (auto&& expression : expressions)
      {
        static_cast<Environment &>(*this).evaluate(expression);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
