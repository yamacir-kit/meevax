#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>
#include <unordered_map>

#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/ghost.hpp>
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

    explicit configurator()
    {}

    Import(SK, evaluate);
    Import(SK, read);

    Import_Const(SK, standard_verbose_port);
    Import_Const(SK, newline);
    Import_Const(SK, write);
    Import_Const(SK, write_to);
    Import_Const(SK, write_line);

    object batch_mode       { f };
    object debug_mode       { f };
    object interactive_mode { f };
    object trace_mode       { f };
    object verbose_mode     { f };

  public:
    static inline const version current_version {};
    static inline const feature current_feature {};

    object paths    { unit };
    object variable { unit };

    #define BOILERPLATE(MODE)                                                  \
    auto in_##MODE() const                                                     \
    {                                                                          \
      return MODE.is<boolean>() and MODE.as<boolean>();                        \
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
      write_line("Meevax Lisp System, version ", current_version.semantic());
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

    auto display_help() const -> decltype(auto)
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
      write_line("  ", BOLD("-h"), ", ", BOLD("--help"), "                 Display version information and exit.");
      write_line("  ", BOLD("-i"), ", ", BOLD("--interactive"), "          Interactive mode: Take over control of root syntactic-continuation.");
      write_line("  ", BOLD("-l"), ", ", BOLD("--load"), "=", UNDERLINE("file"), "            Load ", UNDERLINE("file"), " before main session.");
      write_line("  ", BOLD("-r"), ", ", BOLD("--revised"), "=", UNDERLINE("integer"), "      (unimplemented)");
      write_line("  ", BOLD("-t"), ", ", BOLD("--trace"), "                Trace mode: Display stacks of virtual machine for each instruction.");
      write_line("  ", BOLD("-v"), ", ", BOLD("--version"), "              Display this help text and exit.");
      write_line("  ", BOLD("  "), "  ", BOLD("--verbose"), "              Verbose mode: Display detailed informations.");
      newline();

      SECTION("Sequence:");
      write_line("  1. ", BOLD("Configure"));
      write_line("  2. ", BOLD("Batch operation"), " (for each ", UNDERLINE("file"), " specified)");
      write_line("  3. ", BOLD("Interactive operation"), " (when --interactive specified)");
      newline();

      SECTION("Examples:");
      write_line("  $ meevax -e '(features)'  ; => Display features.");

      #undef SECTION
      #undef BOLD
      #undef UNDERLINE
    }

  public:
    template <typename T>
    using dispatcher = std::unordered_map<T, std::function<PROCEDURE()>>;

    // NOTE
    //   --from=FILE --to=FILE
    //   --input=FILE --output=FILE

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

      std::make_pair('l', [this](const object& s)
      {
        if (s.is<symbol>())
        {
          return paths = cons(make<path>(s.as<const std::string>()), paths);
        }
        else
        {
          return unspecified;
        }
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

      std::make_pair("load", [this](const object& s)
      {
        if (s.is<symbol>())
        {
          return paths = cons(make<path>(s.as<const std::string>()), paths);
        }
        else
        {
          return unspecified;
        }
      }),

      std::make_pair("variable", [this](const auto& xs)
      {
        std::cerr << "; configure\t; " << variable << " => " << (variable = xs) << std::endl;
        return variable;
      }),
    };

  public:
    auto configure(const int argc, char const* const* const argv) -> decltype(auto)
    {
      const std::vector<std::string> options {argv + 1, argv + argc};

      return configure(options);
    }

    void configure(const std::vector<std::string>& args)
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
              if (const std::string rest { std::next(so), std::end(sos) }; rest.length())
              {
                return std::invoke(cdr(*callee), read(*option));
              }
              else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
              {
                return std::invoke(cdr(*callee), read(*option));
              }
              else
              {
                throw error("option -", *so, " requires an argument");
              }
            }
            else if (auto callee { short_options.find(*so) }; callee != std::end(short_options))
            {
              return std::invoke(cdr(*callee), unit);
            }
            else
            {
              throw error("unknown short-option: ", *so);
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
              throw error("option --", lo, " requires an argument");
            }
          }
          else if (auto callee { long_options.find(lo) }; callee != std::end(long_options))
          {
            return std::invoke(cdr(*callee), unit);
          }
          else
          {
            throw error("unknown long-option: ", *option);
          }
        }
        else
        {
          paths = cons(make<path>(*option), paths);
        }

        return unspecified;
      }();

      paths = reverse(paths);

      static const auto rc { path(::getenv("HOME")) / ".meevaxrc" };

      if (in_interactive_mode() and std::experimental::filesystem::exists(rc))
      {
        paths = cons(make<path>(rc), paths);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
