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

    static auto display_version() -> void
    {
      print(version());
    }

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

  private:
    template <typename Key>
    using dispatcher = std::unordered_map<Key, std::function<void (object const&)>>;

    static inline const dispatcher<char> short_options
    {
      std::make_pair('b', [](auto&&...)
      {
        batch = true;
      }),

      std::make_pair('d', [](auto&&...)
      {
        debug = true;
      }),

      std::make_pair('h', [](auto&&...)
      {
        configurator::display_help();
        throw success;
      }),

      std::make_pair('i', [](auto&&...)
      {
        interactive = true;
      }),

      std::make_pair('t', [](auto&&...)
      {
        trace = true;
      }),

      std::make_pair('v', [](auto&&...)
      {
        configurator::display_version();
        throw success;
      }),
    };

    static inline const dispatcher<char> short_options_with_arguments
    {
      std::make_pair('e', [](auto&& x)
      {
        print(interaction_environment().as<Environment>().evaluate(x));
      }),

      std::make_pair('l', [](auto&& x)
      {
        interaction_environment().as<Environment>().load(x.template as_const<symbol>());
      }),

      std::make_pair('w', [](auto&& x)
      {
        print(x);
      }),
    };

    static inline const dispatcher<std::string> long_options
    {
      std::make_pair("batch", [](auto&&...)
      {
        batch = true;
      }),

      std::make_pair("debug", [](auto&&...)
      {
        debug = true;
      }),

      std::make_pair("help", [](auto&&...)
      {
        display_help();
        throw success;
      }),

      std::make_pair("interactive", [](auto&&...)
      {
        interactive = true;
      }),

      std::make_pair("trace", [](auto&&...)
      {
        trace = true;
      }),

      std::make_pair("version", [](auto&&...)
      {
        display_version();
        throw success;
      }),
    };

    static inline const dispatcher<std::string> long_options_with_arguments
    {
      std::make_pair("evaluate", [](auto&& x)
      {
        print(interaction_environment().as<Environment>().evaluate(x));
      }),

      std::make_pair("load", [](auto&& x)
      {
        interaction_environment().as<Environment>().load(x.template as_const<string>());
      }),

      std::make_pair("write", [](auto&& x)
      {
        print(x);
      }),
    };

    struct option
    {
      let const operation;

      bool const supports_abbreviation;

      bool const requires_an_argument;

      template <typename S, typename F>
      explicit option(S&& s, F&& f, bool supports_abbreviation = false, bool requires_an_argument = false)
        : operation { make<procedure>(std::forward<decltype(s)>(s),
                                      std::forward<decltype(f)>(f)) }
        , supports_abbreviation { supports_abbreviation }
        , requires_an_argument { requires_an_argument }
      {}

      auto name() const -> auto const&
      {
        return operation.as<procedure>().name;
      }
    };

  public:
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
        option("help",        [this](let const&) { display_help();     return unit; }, true),
        option("version",     [this](let const&) { display_version();  return unit; }, true),

        option("batch",       [this](let const&) { batch       = true; return unit; }, true),
        option("debug",       [this](let const&) { debug       = true; return unit; }, true),
        option("interactive", [this](let const&) { interactive = true; return unit; }, true),
        option("trace",       [this](let const&) { trace       = true; return unit; }, true),

        option("evaluate", [this](let const& xs)
        {
          return static_cast<Environment &>(*this).evaluate(xs[0]);
        }, true, true),

        option("load", [this](let const& xs)
        {
          static_cast<Environment &>(*this).load(xs[0].as<string>());
          return unit;
        }, true, true),

        option("write", [this](let const& xs)
        {
          std::cout << xs[0] << std::endl;
          return unit;
        }, true, true),
      };

      auto long_option = [&](auto&& name) -> auto const&
      {
        if (auto iter = std::find_if(std::cbegin(options), std::cend(options), [&](auto&& option)
                        {
                          return option.name() == name;
                        });
            iter != std::cend(options))
        {
          return *iter;
        }
        else
        {
          throw error(make<string>("unknown long-option"),
                      make<symbol>(lexical_cast<std::string>("--", name)));
        }
      };

      auto short_option = [&](auto&& name) -> auto const&
      {
        if (auto iter = std::find_if(std::cbegin(options), std::cend(options), [&](auto&& option)
                        {
                          return option.supports_abbreviation and option.name()[0] == name;
                        });
            iter != std::cend(options))
        {
          return *iter;
        }
        else
        {
          throw error(make<string>("unknown short-option"),
                      make<symbol>(lexical_cast<std::string>('-', name)));
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
              if (auto&& option = short_option(str3.front()); option.requires_an_argument)
              {
                expressions.push_back(list(option.operation,
                                           list(quote,
                                                1 < str3.length() ? read(str3.substr(1))
                                                                  : read_argument())));
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
            expressions.push_back(list(long_option(result.str(1)).operation,
                                       list(quote,
                                            read(result.str(2)))));
          }
          else if (result.length(1))
          {
            if (auto&& option = long_option(result.str(1)); option.requires_an_argument)
            {
              expressions.push_back(list(option.operation,
                                         list(quote,
                                              read_argument())));
            }
            else
            {
              expressions.push_back(list(option.operation));
            }
          }
        }
        else
        {
          expressions.push_back(list(long_option("load").operation, make<string>(*iter)));
          interactive = false;
        }
      }

      for (auto&& expression : expressions)
      {
        std::cout << "option = " << expression << std::endl;
        static_cast<Environment &>(*this).evaluate(expression);
      }

      for (auto current_option = std::begin(args); false; ++current_option) [&]()
      {
        std::smatch analysis {};

        std::regex_match(*current_option, analysis, pattern);

        // std::cout << header("configure") << "analysis[0] = " << analysis[0] << std::endl;
        // std::cout << header("")          << "analysis[1] = " << analysis[1] << std::endl;
        // std::cout << header("")          << "analysis[2] = " << analysis[2] << std::endl;
        // std::cout << header("")          << "analysis[3] = " << analysis[3] << std::endl;
        // std::cout << header("")          << "analysis[4] = " << analysis[4] << std::endl;

        if (auto const& current_short_options = analysis.str(4); not current_short_options.empty())
        {
          for (auto current_short_option = std::cbegin(current_short_options); current_short_option != std::cend(current_short_options); ++current_short_option)
          {
            if (auto iter = short_options_with_arguments.find(*current_short_option); iter != std::end(short_options_with_arguments))
            {
              if (auto const& [name, perform] = *iter; std::next(current_short_option) != std::end(current_short_options))
              {
                return perform(read(std::string(std::next(current_short_option), std::end(current_short_options))));
              }
              else if (++current_option != std::end(args) and not std::regex_match(*current_option, analysis, pattern))
              {
                return perform(read(*current_option));
              }
              else
              {
                throw error(make<string>("option requires an argument"),
                            make<symbol>(lexical_cast<std::string>("-", name)));
              }
            }
            else if (auto iter = short_options.find(*current_short_option); iter != std::end(short_options))
            {
              cdr(*iter)(unit);
            }
            else
            {
              throw error(make<string>("unknown option"),
                          make<symbol>(lexical_cast<std::string>("-", *current_short_option)));
            }
          }
        }
        else if (auto const current_long_option = analysis.str(1); not current_long_option.empty())
        {
          if (auto iter = long_options_with_arguments.find(current_long_option); iter != std::cend(long_options_with_arguments))
          {
            if (analysis.length(2)) // argument part
            {
              return cdr(*iter)(read(analysis.str(3)));
            }
            else if (++current_option != std::end(args) and not std::regex_match(*current_option, analysis, pattern))
            {
              return cdr(*iter)(read(*current_option));
            }
            else
            {
              throw error(make<string>("option requires an argument"),
                          make<symbol>(lexical_cast<std::string>("--", current_long_option)));
            }
          }
          else if (auto iter = long_options.find(current_long_option); iter != std::end(long_options))
          {
            return cdr(*iter)(unit);
          }
          else
          {
            throw error(make<string>("unknown option"),
                        make<symbol>(lexical_cast<std::string>("--", current_long_option)));
          }
        }
        else
        {
          interactive = false;
          interaction_environment().as<Environment>().load(*current_option);
        }
      }();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
