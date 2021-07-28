#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename SK>
  class configurator
  {
    friend SK;

    explicit configurator()
    {}

    IMPORT(SK, evaluate, NIL);
    IMPORT(SK, load, NIL);
    IMPORT(SK, newline, const);
    IMPORT(SK, read, NIL);
    IMPORT(SK, write, const);
    IMPORT(SK, write_line, const);

  protected:
    let batch_mode       = f;
    let debug_mode       = f;
    let interactive_mode = f;
    let trace_mode       = f;
    let verbose_mode     = f;

    #define BOILERPLATE(MODE)                                                  \
    auto in_##MODE() const                                                     \
    {                                                                          \
      return not eq(MODE, f);                                                  \
    } static_assert(true)

    BOILERPLATE(batch_mode);
    BOILERPLATE(debug_mode);
    BOILERPLATE(interactive_mode);
    BOILERPLATE(trace_mode);
    BOILERPLATE(verbose_mode);

    #undef BOILERPLATE

    template <typename Key>
    using dispatcher = std::unordered_map<Key, std::function<PROCEDURE()>>;

    const dispatcher<char> short_options
    {
      std::make_pair('b', [this](auto&&...)
      {
        return batch_mode = t;
      }),

      std::make_pair('d', [this](auto&&...)
      {
        return debug_mode = t;
      }),

      std::make_pair('h', [this](auto&&...)
      {
        display_help();
        return std::exit(boost::exit_success), unspecified;
      }),

      std::make_pair('i', [this](auto&&...)
      {
        return interactive_mode = t;
      }),

      std::make_pair('v', [this](auto&&...)
      {
        display_version();
        return std::exit(boost::exit_success), unspecified;
      }),
    };

    const dispatcher<char> short_options_
    {
      std::make_pair('e', [&](auto&&... xs)
      {
        std::cout << evaluate(std::forward<decltype(xs)>(xs)...) << std::endl;
        return unspecified;
      }),

      std::make_pair('l', [this](auto&&... xs)
      {
        return load(std::forward<decltype(xs)>(xs)...);
      }),

      std::make_pair('w', [this](auto&&... xs)
      {
        return write(std::forward<decltype(xs)>(xs)...), unspecified;
      }),
    };

    const dispatcher<std::string> long_options
    {
      std::make_pair("batch", [this](auto&&...)
      {
        return batch_mode = t;
      }),

      std::make_pair("debug", [this](auto&&...)
      {
        return debug_mode = t;
      }),

      std::make_pair("help", [this](auto&&...)
      {
        display_help();
        return std::exit(boost::exit_success), unspecified;
      }),

      std::make_pair("interactive", [this](auto&&...)
      {
        return interactive_mode = t;
      }),

      std::make_pair("trace", [this](auto&&...)
      {
        return trace_mode = t;
      }),

      std::make_pair("verbose", [this](auto&&...)
      {
        return verbose_mode = t;
      }),

      std::make_pair("version", [this](auto&&...)
      {
        display_version();
        newline();
        display_license();
        return std::exit(boost::exit_success), unspecified;
      }),
    };

    const dispatcher<std::string> long_options_
    {
      std::make_pair("evaluate", [&](auto&&... xs)
      {
        std::cout << evaluate(std::forward<decltype(xs)>(xs)...) << std::endl;
        return unspecified;
      }),

      std::make_pair("load", [this](auto&&... xs)
      {
        return load(std::forward<decltype(xs)>(xs)...);
      }),

      std::make_pair("write", [this](auto&&... xs)
      {
        return write(std::forward<decltype(xs)>(xs)...), unspecified;
      }),
    };

  public:
    auto configure(const int argc, char const* const* const argv)
    {
      std::vector<std::string> const options { argv + 1, argv + argc };
      return configure(options);
    }

    void configure(std::vector<std::string> const& args)
    {
      static std::regex const pattern { R"(--(\w[-\w]+)(=(.*))?|-([\w]+))" };

      for (auto option = std::begin(args); option != std::end(args); ++option) [&]()
      {
        std::smatch analysis {};

        std::regex_match(*option, analysis, pattern);

        if (in_debug_mode())
        {
          std::cout << header("configure") << "analysis[0] = " << analysis[0] << std::endl;
          std::cout << header("")          << "analysis[1] = " << analysis[1] << std::endl;
          std::cout << header("")          << "analysis[2] = " << analysis[2] << std::endl;
          std::cout << header("")          << "analysis[3] = " << analysis[3] << std::endl;
          std::cout << header("")          << "analysis[4] = " << analysis[4] << std::endl;
        }

        if (const auto current_short_options = analysis.str(4); not current_short_options.empty())
        {
          for (auto current_short_option = std::cbegin(current_short_options); current_short_option != std::cend(current_short_options); ++current_short_option)
          {
            if (auto iter = short_options_.find(*current_short_option); iter != std::end(short_options_))
            {
              auto const& [name, perform] = *iter;

              if (auto const rest = std::string(std::next(current_short_option), std::end(current_short_options)); not rest.empty())
              {
                return perform(read(rest));
              }
              else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
              {
                return perform(read(*option));
              }
              else
              {
                throw error(make<string>(string_append("option -", name, " requires an argument")), unit);
              }
            }
            else if (auto callee = short_options.find(*current_short_option); callee != std::end(short_options))
            {
              std::invoke(cdr(*callee), unit);
            }
            else
            {
              throw error(make<string>(string_append("unknown short-option -", *current_short_option)), unit);
            }
          }
        }
        else if (const auto lo { analysis.str(1) }; lo.length())
        {
          if (auto callee { long_options_.find(lo) }; callee != std::end(long_options_))
          {
            if (analysis.length(2)) // argument part
            {
              return std::invoke(cdr(*callee), read(analysis.str(3)));
            }
            else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
            {
              return std::invoke(cdr(*callee), read(*option));
            }
            else
            {
              throw error(make<string>(string_append("option --", lo, " requires an argument")), unit);
            }
          }
          else if (auto callee { long_options.find(lo) }; callee != std::end(long_options))
          {
            return std::invoke(cdr(*callee), unit);
          }
          else
          {
            throw error(make<string>(string_append("unknown long-option: ", *option)), unit);
          }
        }
        else
        {
          return load(*option);
        }

        return unspecified;
      }();
    }

    void display_version() const
    {
      write_line("Meevax Lisp System, version ", version());
    }

    void display_license() const
    {
      write_line("   Copyright 2020 Tatsuya Yamasaki");
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
      write_line("  -l, --load=FILE        same as -e '(load FILE)'");
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
      write_line("  meevax -e '(+ 1 2 3)'");
      write_line("    => display 6.");
      write_line();
      write_line("  meevax -e \"(define home \\\"$HOME\\\")\" -i");
      write_line("    => define value of environment variable $HOME to interaction-environment,");
      write_line("       and then start interactive session.");
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
