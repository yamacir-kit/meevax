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
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename environment>
  class configurator
  {
    friend environment;

    IMPORT(environment, evaluate, NIL);
    IMPORT(environment, load, NIL);
    IMPORT(environment, print, const);
    IMPORT(environment, read, NIL);

    template <typename Key>
    using dispatcher = std::unordered_map<Key, procedure::function_type>;

    const dispatcher<char> short_options, short_options_with_arguments;

    const dispatcher<std::string> long_options, long_options_with_arguments;

  public:
    bool batch       = false;
    bool debug       = false;
    bool interactive = false;
    bool trace       = false;
    bool verbose     = false;

    explicit configurator()
      : short_options
        {
          std::make_pair('b', [this](auto&&...)
          {
            return make<bool>(batch = true);
          }),

          std::make_pair('d', [this](auto&&...)
          {
            return make<bool>(debug = true);
          }),

          std::make_pair('h', [this](auto&&...) -> lvalue
          {
            display_help();
            throw exit_status::success;
          }),

          std::make_pair('i', [this](auto&&...)
          {
            return make<bool>(interactive = true);
          }),

          std::make_pair('v', [this](auto&&...) -> lvalue
          {
            display_version();
            throw exit_status::success;
          }),
        }

      , short_options_with_arguments
        {
          std::make_pair('e', [this](const_reference x, auto&&...)
          {
            return print(evaluate(x)), unspecified_object;
          }),

          std::make_pair('l', [this](const_reference x, auto&&...)
          {
            return load(x.as_const<string>());
          }),

          std::make_pair('w', [this](const_reference x, auto&&...)
          {
            return print(x), unspecified_object;
          }),
        }

      , long_options
        {
          std::make_pair("batch", [this](auto&&...)
          {
            return make<bool>(batch = true);
          }),

          std::make_pair("debug", [this](auto&&...)
          {
            return make<bool>(debug = true);
          }),

          std::make_pair("help", [this](auto&&...) -> lvalue
          {
            display_help();
            throw exit_status::success;
          }),

          std::make_pair("interactive", [this](auto&&...)
          {
            return make<bool>(interactive = true);
          }),

          std::make_pair("trace", [this](auto&&...)
          {
            return make<bool>(trace = true);
          }),

          std::make_pair("verbose", [this](auto&&...)
          {
            return make<bool>(verbose = true);
          }),

          std::make_pair("version", [this](auto&&...) -> lvalue
          {
            display_version();
            throw exit_status::success;
          }),
        }

      , long_options_with_arguments
        {
          std::make_pair("evaluate", [this](const_reference x, auto&&...)
          {
            return print(evaluate(x)), unspecified_object;
          }),

          std::make_pair("load", [this](const_reference x, auto&&...)
          {
            return load(x.as_const<string>());
          }),

          std::make_pair("write", [this](const_reference x, auto&&...)
          {
            return print(x), unspecified_object;
          }),
        }
    {}

    auto configure(const int argc, char const* const* const argv)
    {
      return configure({ argv + 1, argv + argc });
    }

    auto configure(std::vector<std::string> const& args) -> void
    {
      static std::regex const pattern { R"(--(\w[-\w]+)(=(.*))?|-([\w]+))" };

      if (std::empty(args))
      {
        interactive = true;
      }
      else for (auto current_option = std::begin(args); current_option != std::end(args); ++current_option) [&]()
      {
        std::smatch analysis {};

        std::regex_match(*current_option, analysis, pattern);

        // std::cout << header("configure") << "analysis[0] = " << analysis[0] << std::endl;
        // std::cout << header("")          << "analysis[1] = " << analysis[1] << std::endl;
        // std::cout << header("")          << "analysis[2] = " << analysis[2] << std::endl;
        // std::cout << header("")          << "analysis[3] = " << analysis[3] << std::endl;
        // std::cout << header("")          << "analysis[4] = " << analysis[4] << std::endl;

        if (auto const current_short_options = analysis.str(4); not current_short_options.empty())
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
                throw error(make<string>(cat, "option -", name, " requires an argument"));
              }
            }
            else if (auto iter = short_options.find(*current_short_option); iter != std::end(short_options))
            {
              cdr(*iter)(unit);
            }
            else
            {
              throw error(make<string>(cat, "unknown short-option -", *current_short_option));
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
              throw error(make<string>(cat, "option --", current_long_option, " requires an argument"));
            }
          }
          else if (auto iter = long_options.find(current_long_option); iter != std::end(long_options))
          {
            return cdr(*iter)(unit);
          }
          else
          {
            throw error(make<string>(cat, "unknown long-option: ", *current_option));
          }
        }
        else
        {
          return load(*current_option);
        }

        return unspecified_object;
      }();
    }

    auto display_version() const -> void
    {
      print("Meevax Lisp ", version());
    }

    auto display_help() const -> void
    {
      display_version();
      print();
      print("Usage: meevax [OPTION...] [FILE...]");
      print();
      print("Options:");
      print("  -b, --batch            Suppress any system output.");
      print("  -d, --debug            Display detailed informations for developers.");
      print("  -e, --evaluate=STRING  Read and evaluate given STRING at configuration step.");
      print("  -h, --help             Display this help text and exit.");
      print("  -i, --interactive      Take over control of root environment.");
      print("  -l, --load=FILENAME    Same as -e '(load FILENAME)'");
      print("  -t, --trace            Display stacks of virtual machine for each steps.");
      print("  -v, --version          Display version information and exit.");
      print("      --verbose          Display detailed informations.");
      print("  -w, --write=OBJECT     Same as -e '(write OBJECT)'");
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
