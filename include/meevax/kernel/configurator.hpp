/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename SK>
  class configurator
  {
    friend SK;

    IMPORT(SK, evaluate, NIL);
    IMPORT(SK, load, NIL);
    IMPORT(SK, newline, const);
    IMPORT(SK, read, NIL);
    IMPORT(SK, write, const);
    IMPORT(SK, write_line, const);

  protected:
    let batch       = f;
    let debug       = f;
    let interactive = f;
    let trace       = f;
    let verbose     = f;

    let prompt = make<string>(u8"λ> ");

    template <typename Key>
    using dispatcher = std::unordered_map<Key, procedure::function>;

    const dispatcher<char> short_options, short_options_with_arguments;

    const dispatcher<std::string> long_options, long_options_with_arguments;

  public:
    explicit configurator()
      : short_options
        {
          std::make_pair('b', [this](auto&&...)
          {
            return batch = t;
          }),

          std::make_pair('d', [this](auto&&...)
          {
            return debug = t;
          }),

          std::make_pair('h', [this](auto&&...)
          {
            display_help();
            throw exit_status::success;
            return unspecified;
          }),

          std::make_pair('i', [this](auto&&...)
          {
            return interactive = t;
          }),

          std::make_pair('v', [this](auto&&...)
          {
            display_version();
            throw exit_status::success;
            return unspecified;
          }),
        }

      , short_options_with_arguments
        {
          std::make_pair('e', [this](pair::const_reference x)
          {
            return write_line(evaluate(x)), unspecified;
          }),

          std::make_pair('l', [this](pair::const_reference x)
          {
            return load(x);
          }),

          std::make_pair('w', [this](pair::const_reference x)
          {
            return write(x), unspecified;
          }),
        }

      , long_options
        {
          std::make_pair("batch", [this](auto&&...)
          {
            return batch = t;
          }),

          std::make_pair("debug", [this](auto&&...)
          {
            return debug = t;
          }),

          std::make_pair("help", [this](auto&&...)
          {
            display_help();
            throw exit_status::success;
            return unspecified;
          }),

          std::make_pair("interactive", [this](auto&&...)
          {
            return interactive = t;
          }),

          std::make_pair("trace", [this](auto&&...)
          {
            return trace = t;
          }),

          std::make_pair("verbose", [this](auto&&...)
          {
            return verbose = t;
          }),

          std::make_pair("version", [this](auto&&...)
          {
            display_version();
            newline();
            display_license();
            throw exit_status::success;
            return unspecified;
          }),
        }

      , long_options_with_arguments
        {
          std::make_pair("evaluate", [this](pair::const_reference x)
          {
            return write_line(evaluate(x)), unspecified;
          }),

          std::make_pair("load", [this](pair::const_reference x)
          {
            return load(x);
          }),

          std::make_pair("prompt", [this](pair::const_reference x)
          {
            return prompt = x;
          }),

          std::make_pair("write", [this](pair::const_reference x)
          {
            return write(x), unspecified;
          }),
        }
    {}

    auto configure(const int argc, char const* const* const argv)
    {
      return configure({ argv + 1, argv + argc });
    }

    void configure(std::vector<std::string> const& args)
    {
      static std::regex const pattern { R"(--(\w[-\w]+)(=(.*))?|-([\w]+))" };

      for (auto current_option = std::begin(args); current_option != std::end(args); ++current_option) [&]()
      {
        std::smatch analysis {};

        std::regex_match(*current_option, analysis, pattern);

        if (is_debug_mode())
        {
          std::cout << header("configure") << "analysis[0] = " << analysis[0] << std::endl;
          std::cout << header("")          << "analysis[1] = " << analysis[1] << std::endl;
          std::cout << header("")          << "analysis[2] = " << analysis[2] << std::endl;
          std::cout << header("")          << "analysis[3] = " << analysis[3] << std::endl;
          std::cout << header("")          << "analysis[4] = " << analysis[4] << std::endl;
        }

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
                throw error(make<string>(string_append("option -", name, " requires an argument")), unit);
              }
            }
            else if (auto iter = short_options.find(*current_short_option); iter != std::end(short_options))
            {
              std::get<1>(*iter)(unit);
            }
            else
            {
              throw error(make<string>(string_append("unknown short-option -", *current_short_option)), unit);
            }
          }
        }
        else if (auto const current_long_option = analysis.str(1); not current_long_option.empty())
        {
          if (auto iter = long_options_with_arguments.find(current_long_option); iter != std::cend(long_options_with_arguments))
          {
            if (analysis.length(2)) // argument part
            {
              return std::get<1>(*iter)(read(analysis.str(3)));
            }
            else if (++current_option != std::end(args) and not std::regex_match(*current_option, analysis, pattern))
            {
              return std::get<1>(*iter)(read(*current_option));
            }
            else
            {
              throw error(make<string>(string_append("option --", current_long_option, " requires an argument")), unit);
            }
          }
          else if (auto iter = long_options.find(current_long_option); iter != std::end(long_options))
          {
            return std::get<1>(*iter)(unit);
          }
          else
          {
            throw error(make<string>(string_append("unknown long-option: ", *current_option)), unit);
          }
        }
        else
        {
          return load(*current_option);
        }

        return unspecified;
      }();
    }

    auto current_prompt() const
    {
      return static_cast<std::string>(prompt.as<string>());
    }

    void display_version() const
    {
      write_line("Meevax Lisp System, version ", version());
    }

    void display_license() const
    {
      write_line("   Copyright 2018-2021 Tatsuya Yamasaki.");
      write_line();
      write_line("   Licensed under the Apache License, Version 2.0 (the \"License\");");
      write_line("   you may not use this file except in compliance with the License.");
      write_line("   You may obtain a copy of the License at");
      write_line();
      write_line("       http://www.apache.org/licenses/LICENSE-2.0");
      write_line();
      write_line("   Unless required by applicable law or agreed to in writing, software");
      write_line("   distributed under the License is distributed on an \"AS IS\" BASIS,");
      write_line("   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.");
      write_line("   See the License for the specific language governing permissions and");
      write_line("   limitations under the License.");
    }

    void display_help() const
    {
      display_version();
      write_line();
      write_line("Usage: meevax [OPTION...] [FILE...]");
      write_line();
      write_line("Options:");
      write_line("  -b, --batch            suppress any system output.");
      write_line("  -d, --debug            display detailed informations for developers.");
      write_line("  -e, --evaluate=STRING  read and evaluate given STRING at configuration step.");
      write_line("  -h, --help             display this help text and exit.");
      write_line("  -i, --interactive      take over control of root syntactic-continuation.");
      write_line("  -l, --load=FILENAME    same as -e '(load FILENAME)'");
      write_line("      --prompt=STRING    same as -e '(set-prompt! STRING)'");
      write_line("  -t, --trace            display stacks of virtual machine for each steps.");
      write_line("  -v, --version          display version information and exit.");
      write_line("      --verbose          display detailed informations.");
      write_line("  -w, --write=OBJECT     same as -e '(write OBJECT)'");
      write_line();
      write_line("Boot Sequence:");
      write_line("  1. Load basis libraries");
      write_line("  2. Configure");
      write_line("  3. Load given FILEs");
      write_line("  4. Interaction (if --interactive specified)");
      write_line();
      write_line("Examples:");
      write_line("  meevax -i");
      write_line("    => start interactive session.");
      write_line();
      write_line("  meevax -i --prompt '\"my-prompt> \"'");
      write_line("    => Start interactive session with given prompt.");
      write_line();
      write_line("  meevax -e '(+ 1 2 3)'");
      write_line("    => Display 6.");
      write_line();
      write_line("  meevax -e \"(define home \\\"$HOME\\\")\" -i");
      write_line("    => Define value of environment variable $HOME to interaction-environment,");
      write_line("       and then start interactive session.");
    }

    #define BOILERPLATE(NAME)                                                  \
    auto is_##NAME##_mode() const -> bool                                      \
    {                                                                          \
      return if_(NAME);                                                        \
    } static_assert(true)

    BOILERPLATE(batch);
    BOILERPLATE(debug);
    BOILERPLATE(interactive);
    BOILERPLATE(trace);
    BOILERPLATE(verbose);

    #undef BOILERPLATE
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
