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
    IMPORT(SK, newline, const);
    IMPORT(SK, read, NIL);
    IMPORT(SK, standard_verbose_port, const);
    IMPORT(SK, write, const);
    IMPORT(SK, write_line, const);
    IMPORT(SK, write_to, const);

  protected:
    let batch_mode       = f;
    let debug_mode       = f;
    let interactive_mode = f;
    let trace_mode       = f;
    let verbose_mode     = f;

  public:
    let paths = unit;

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

  public:
    let display_version() const
    {
      write_line("Meevax Lisp System, version ", version());
      return unspecified;
    }

    let display_license() const
    {
      write(
        "Copyright (C) 2020 Tatsuya Yamasaki\n"
        "\n"
        "Licensed under the Apache License, Version 2.0 (the \"License\");\n"
        "you may not use this file except in compliance with the License.\n"
        "You may obtain a copy of the License at\n"
        "\n"
        "    http://www.apache.org/licenses/LICENSE-2.0\n"
        "\n"
        "Unless required by applicable law or agreed to in writing, software\n"
        "distributed under the License is distributed on an \"AS IS\" BASIS,\n"
        "WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n"
        "See the License for the specific language governing permissions and\n"
        "limitations under the License.\n");

      return unspecified;
    }

    auto display_help() const
    {
      display_version();
      newline();

      #define SECTION(NAME) write_line(bold, NAME)
      #define BOLD(...) bold, __VA_ARGS__, reset
      #define UNDERLINE(...) underline, __VA_ARGS__, reset

      SECTION("Usage:");
      write_line("  ", BOLD("meevax"), " [", UNDERLINE("option"), "]... [", UNDERLINE("file"), "]...");
      newline();

      SECTION("Options:");
      write_line("  ", BOLD("-b"), ", ", BOLD("--batch"), "                Batch mode: Suppress any system output.");
      write_line("  ", BOLD("-d"), ", ", BOLD("--debug"), "                Debug mode: Display detailed informations for developers.");
      write_line("  ", BOLD("-e"), ", ", BOLD("--evaluate"), "=", UNDERLINE("expression"), "  Evaluate an ", UNDERLINE("expression"), " at configuration time.");
      write_line("  ", BOLD("  "), "  ", BOLD("--echo"), "=", UNDERLINE("expression"), "      Write ", UNDERLINE("expression"), ".");
      write_line("  ", BOLD("-f"), ", ", BOLD("--feature"), "=", UNDERLINE("identifier"), "   (unimplemented)");
      write_line("  ", BOLD("-h"), ", ", BOLD("--help"), "                 Display this help text and exit.");
      write_line("  ", BOLD("-i"), ", ", BOLD("--interactive"), "          Interactive mode: Take over control of root syntactic-continuation.");
      write_line("  ", BOLD("-l"), ", ", BOLD("--load"), "=", UNDERLINE("file"), "            Load ", UNDERLINE("file"), " before main session.");
      write_line("  ", BOLD("-r"), ", ", BOLD("--revised"), "=", UNDERLINE("integer"), "      (unimplemented)");
      write_line("  ", BOLD("-t"), ", ", BOLD("--trace"), "                Trace mode: Display stacks of virtual machine for each instruction.");
      write_line("  ", BOLD("-v"), ", ", BOLD("--version"), "              Display version information and exit.");
      write_line("  ", BOLD("  "), "  ", BOLD("--verbose"), "              Verbose mode: Display detailed informations.");
      newline();

      SECTION("Sequence:");
      write_line("  1. ", BOLD("Boot"));
      write_line("  2. ", BOLD("Configure"));
      write_line("  3. ", BOLD("Load"), " (for each ", UNDERLINE("file"), " specified)");
      write_line("  4. ", BOLD("REPL"), " (when --interactive specified)");
      newline();

      SECTION("Examples:");
      write_line("  $ meevax -e '(features)'  ; => Display features.");

      #undef SECTION
      #undef BOLD
      #undef UNDERLINE
    }

    let const& append_path(let const& x)
    {
      if (x.is<symbol>())
      {
        return push(paths, make<path>(x.as<std::string>()));
      }
      else
      {
        return unspecified;
      }
    }

  public:
    template <typename T>
    using dispatcher = std::unordered_map<T, std::function<PROCEDURE()>>;

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
        newline();
        display_license();
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
        return append_path(std::forward<decltype(xs)>(xs)...);
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

      // TODO --srfi=0,1,2
      // TODO --reviced=4,5,7

      std::make_pair("trace", [this](auto&&...)
      {
        return trace_mode = t;
      }),

      std::make_pair("verbose", [this](auto&&...)
      {
        return static_cast<SK&>(*this).verbose_mode = t;
      }),

      std::make_pair("version", [this](auto&&...)
      {
        display_version();
        return std::exit(boost::exit_success), unspecified;
      }),
    };

    const dispatcher<std::string> long_options_
    {
      std::make_pair("echo", [&](const auto& xs)
      {
        write_line(xs);
        return unspecified;
      }),

      std::make_pair("evaluate", [&](auto&&... xs)
      {
        std::cout << evaluate(std::forward<decltype(xs)>(xs)...) << std::endl;
        return unspecified;
      }),

      std::make_pair("load", [this](auto&&... xs)
      {
        return append_path(std::forward<decltype(xs)>(xs)...);
      }),
    };

  public:
    auto configure(const int argc, char const* const* const argv) -> decltype(auto)
    {
      const std::vector<std::string> options {argv + 1, argv + argc};

      return configure(options);
    }

    void configure(std::vector<std::string> const& args)
    {
      static const std::regex pattern { "--([[:alnum:]][-_[:alnum:]]+)(=(.*))?|-([[:alnum:]]+)" };

      for (auto option {std::begin(args)}; option != std::end(args); ++option) [&]()
      {
        std::smatch analysis {};

        std::regex_match(*option, analysis, pattern);

        if (const auto sos { analysis.str(4) }; sos.length()) // short-options
        {
          for (auto so { std::begin(sos) }; so != std::end(sos); ++so) // each short-option
          {
            if (auto callee { short_options_.find(*so) }; callee != std::end(short_options_))
            {
              if (std::string const rest { std::next(so), std::end(sos) }; rest.length())
              {
                return std::invoke(cdr(*callee), read(*option));
              }
              else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
              {
                return std::invoke(cdr(*callee), read(*option));
              }
              else
              {
                throw error(
                  make<string>(string_append("option -", *so, " requires an argument")),
                  unit);
              }
            }
            else if (auto callee { short_options.find(*so) }; callee != std::end(short_options))
            {
              return std::invoke(cdr(*callee), unit);
            }
            else
            {
              throw error(
                make<string>(string_append("unknown short-option: ", *so)),
                unit);
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
              throw error(
                make<string>(string_append("option --", lo, " requires an argument")),
                unit);
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
          paths = cons(make<path>(*option), paths);
        }

        return unspecified;
      }();

      paths = reverse(paths);

      if (auto const rc = path(::getenv("HOME")) / ".meevaxrc"; in_interactive_mode() and std::experimental::filesystem::exists(rc))
      {
        paths = cons(make<path>(rc), paths);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
