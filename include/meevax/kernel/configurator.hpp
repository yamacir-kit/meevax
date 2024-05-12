/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct configurator
  {
    struct option
    {
      std::regex const pattern;

      std::function<object (std::function<object ()> const&)> build;

      template <typename S, typename F>
      explicit option(S&& s, F&& f)
        : pattern { std::forward<decltype(s)>(s) }
        , build   { std::forward<decltype(f)>(f) }
      {}
    };

    bool interactive = false;

    std::vector<std::string> command_line;

    auto configure(int const argc, char const* const* const argv)
    {
      for (auto i = 0; i < argc; ++i)
      {
        command_line.emplace_back(argv[i]);
      }

      return configure(command_line);
    }

    auto configure(std::vector<std::string> const& args) -> void
    {
      static auto const pattern = std::regex(R"(--(\w[-\w]+)(=(.*))?|-([\w]+))");

      auto const options = std::vector<option>
      {
        option("(i|interactive)", [this](auto)
        {
          let const f = make<procedure>(__func__, [this](let const&)
          {
            interactive = true;
            return unspecified;
          });

          return list(f);
        }),

        option("(e|evaluate)", [](auto read)
        {
          return read();
        }),

        option("(h|help)", [](auto)
        {
          let static const f = make<procedure>(__func__, [](let const&)
          {
            std::cout << help() << std::endl;
            throw EXIT_SUCCESS;
          });

          return list(f);
        }),

        option("(l|load)", [this](auto read)
        {
          let const f = make<procedure>(__func__, [this](let const& xs)
          {
            static_cast<Environment &>(*this).load(car(xs).as<string>());
            return unspecified;
          });

          return list(f, read());
        }),

        option("(v|version)", [](auto)
        {
          let static const f = make<procedure>(__func__, [](let const&)
          {
            std::cout << version() << std::endl;
            throw EXIT_SUCCESS;
          });

          return list(f);
        }),

        option("(w|write)", [](auto read)
        {
          let static const f = make<procedure>(__func__, [](let const& xs)
          {
            std::cout << car(xs) << std::endl;
          });

          return list(f, read());
        }),
      };

      auto search = [&](auto&& name) -> decltype(auto)
      {
        if (auto iter = std::find_if(options.begin(), options.end(), [&](auto&& option)
                        {
                          return std::regex_match(name, option.pattern);
                        });
            iter != options.end())
        {
          return *iter;
        }
        else
        {
          throw error(make<string>("unknown option"), make<symbol>(name));
        }
      };

      std::vector<object> expressions {};

      for (auto iter = std::next(args.begin()); iter != args.end(); ++iter)
      {
        static std::regex const pattern { R"(--(\w[-\w]+)(?:=(.*))?|-([\w]+))" };

        if (std::smatch result; std::regex_match(*iter, result, pattern))
        {
          auto read = [&]()
          {
            if (std::next(iter) != args.end())
            {
              return input_string_port(*++iter).read();
            }
            else
            {
              throw error(make<string>("an argument required but not specified"),
                          make<symbol>(*iter));
            }
          };

          if (result.length(3))
          {
            for (auto str3 = result.str(3); not str3.empty(); str3.erase(0, 1))
            {
              expressions.push_back(search(str3.substr(0, 1)).build(read));
            }
          }
          else if (result.length(2))
          {
            auto read = [result]()
            {
              return input_string_port(result.str(2)).read();
            };

            expressions.push_back(search(result.str(1)).build(read));
          }
          else if (result.length(1))
          {
            expressions.push_back(search(result.str(1)).build(read));
          }
        }
        else
        {
          let const f = make<procedure>(__func__, [iter](let const&)
          {
            Environment().load(*iter);
            return unspecified;
          });

          expressions.push_back(list(f));
        }
      }

      for (let const& expression : expressions)
      {
        static_cast<Environment &>(*this).evaluate(expression);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
