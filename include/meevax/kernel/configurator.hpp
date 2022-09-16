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
#include <meevax/kernel/version.hpp>
#include <meevax/kernel/writer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename environment>
  class configurator
  {
    friend environment;

    explicit configurator()
    {}

    IMPORT(environment, evaluate, NIL);
    IMPORT(environment, load, NIL);
    IMPORT(environment, read, NIL);

    template <typename Key>
    using dispatcher = std::unordered_map<Key, std::function<void (const_reference)>>;

  public:
    static inline auto batch       = false;
    static inline auto debug       = false;
    static inline auto interactive = true;
    static inline auto trace       = false;

    static auto display_version() -> void
    {
      print("Meevax Lisp ", version());
    }

    static auto display_help() -> void
    {
      display_version();
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
        throw exit_status::success;
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
        throw exit_status::success;
      }),
    };

    static inline const dispatcher<char> short_options_with_arguments
    {
      std::make_pair('e', [](auto&& x)
      {
        print(interaction_environment().as<environment>().evaluate(x));
      }),

      std::make_pair('l', [](auto&& x)
      {
        interaction_environment().as<environment>().load(x.template as_const<symbol>());
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
        throw exit_status::success;
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
        throw exit_status::success;
      }),
    };

    static inline const dispatcher<std::string> long_options_with_arguments
    {
      std::make_pair("evaluate", [](auto&& x)
      {
        print(interaction_environment().as<environment>().evaluate(x));
      }),

      std::make_pair("load", [](auto&& x)
      {
        interaction_environment().as<environment>().load(x.template as_const<string>());
      }),

      std::make_pair("write", [](auto&& x)
      {
        print(x);
      }),
    };

  public:
    auto configure(const int argc, char const* const* const argv)
    {
      return configure({ argv + 1, argv + argc });
    }

    auto configure(std::vector<std::string> const& args) -> void
    {
      static std::regex const pattern { R"(--(\w[-\w]+)(=(.*))?|-([\w]+))" };

      for (auto current_option = std::begin(args); current_option != std::end(args); ++current_option) [&]()
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
                throw error(make<string>(concatenate("option -", name, " requires an argument")));
              }
            }
            else if (auto iter = short_options.find(*current_short_option); iter != std::end(short_options))
            {
              cdr(*iter)(unit);
            }
            else
            {
              throw error(make<string>("unknown short-option"),
                          make<symbol>(*current_short_option));
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
              throw error(make<string>(concatenate("option --", current_long_option, " requires an argument")));
            }
          }
          else if (auto iter = long_options.find(current_long_option); iter != std::end(long_options))
          {
            return cdr(*iter)(unit);
          }
          else
          {
            throw error(make<string>("unknown long-option"),
                        make<symbol>(*current_option));
          }
        }
        else
        {
          interactive = false;
          load(*current_option);
        }
      }();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
