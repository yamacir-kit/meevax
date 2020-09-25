#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>
#include <unordered_map>

#include <meevax/functional/curry.hpp>
#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax { inline namespace kernel
{
  template <typename SK>
  class configurator
  {
    friend SK;

    explicit configurator()
    {}

    Import(SK, evaluate);

    Import_Const(SK, current_verbose_port);
    Import_Const(SK, newline);
    Import_Const(SK, write);
    Import_Const(SK, write_to);
    Import_Const(SK, writeln);

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
    auto display_version() const -> decltype(auto)
    {
      write("Meevax Lisp System, version ", current_version.semantic(), "\n");

      return unspecified;
    }

    auto display_version_() const -> const auto&
    {
      write( //  10        20        30        40        50        60        70        80\n"
        "; Meevax Lisp System ", current_version.major(), " - Revision ", current_version.minor(), " Patch ", current_version.patch(), "\n"
        "; ------------------------------------------------------------------------------\n"
        "; version               ; ", current_version.semantic(),                       "\n"
        );

      write_to(current_verbose_port(),
        "; license               ; ", unspecified,                                      "\n"
        ";\n"
        "; build-date            ; ", current_feature.build_date(),                     "\n"
        "; build-hash            ; ", current_feature.build_hash(),                     "\n"
        "; build-type            ; ", current_feature.build_type(),                     "\n"
        ";\n"
        "; cxx-compiler          ; ", current_feature.cxx_compiler(),                   "\n"
        "; cxx-flags             ; ", current_feature.cxx_flags(),                      "\n"
        "; cxx-standard          ; ", current_feature.cxx_standard(),                   "\n"
        ";\n"
        "; system-processor      ; ", current_feature.system_processor(),               "\n"
        "; system-name           ; ", current_feature.system_name(),                    "\n"
        ";\n"
        "; install-prefix        ; ", current_feature.install_prefix(),                 "\n"
        ";\n"
        "; libraries             ; ", current_version.libraries(),                      "\n"
        ";\n"
        );

      write(
        "; feature               ; ", current_feature, "\n"
        );

      return unspecified;
    }

    auto display_help() const -> decltype(auto)
    {
      display_version();
      newline();

      #define SECTION(NAME) writeln(bold, NAME)
      #define BOLD(...) bold, __VA_ARGS__, reset
      #define UNDERLINE(...) underline, __VA_ARGS__, reset

      SECTION("Usage:");
      writeln("  ", BOLD("meevax"), " [", UNDERLINE("option"), "]... [", UNDERLINE("file"), "]...");
      newline();

      SECTION("Options:");
      writeln("  ", BOLD("-b"), ", ", BOLD("--batch"), "                Batch mode: Suppress any system output.");
      writeln("  ", BOLD("-d"), ", ", BOLD("--debug"), "                Debug mode: Display detailed information for developers.");
      writeln("  ", BOLD("-e"), ", ", BOLD("--evaluate"), "=", UNDERLINE("expression"), "  Evaluate an ", UNDERLINE("expression"), " at configuration time.");
      writeln("  ", BOLD("  "), "  ", BOLD("--echo"), "=", UNDERLINE("expression"), "      Write ", UNDERLINE("expression"), ".");
      writeln("  ", BOLD("-f"), ", ", BOLD("--feature"), "=", UNDERLINE("identifier"), "   (unimplemented)");
      writeln("  ", BOLD("-h"), ", ", BOLD("--help"), "                 Display version information and exit.");
      writeln("  ", BOLD("-i"), ", ", BOLD("--interactive"), "          Interactive mode: Take over control of root syntactic-continuation.");
      writeln("  ", BOLD("-l"), ", ", BOLD("--load"), "=", UNDERLINE("file"), "            Load ", UNDERLINE("file"), " before main session.");
      writeln("  ", BOLD("-r"), ", ", BOLD("--revised"), "=", UNDERLINE("integer"), "      (unimplemented)");
      writeln("  ", BOLD("-t"), ", ", BOLD("--trace"), "                Trace mode: Display stacks of virtual machine for each instruction.");
      writeln("  ", BOLD("-v"), ", ", BOLD("--version"), "              Display this help text and exit.");
      newline();

      SECTION("Sequence:");
      writeln("  1. ", BOLD("configuration"));
      writeln("  2. ", BOLD("batch operation"), " (for each ", UNDERLINE("file"), " specified)");
      writeln("  3. ", BOLD("Interactive operation"), " (when --interactive specified)");
      newline();

      SECTION("Examples:");
      writeln("  $ meevax -e '(features)'  => Display features.");

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
      std::make_pair('b', [this](auto&&...) mutable
      {
        return batch_mode = t;
      }),

      std::make_pair('d', [this](auto&&...) mutable
      {
        return debug_mode = t;
      }),

      std::make_pair('h', [this](auto&&...)
      {
        display_help();
        return std::exit(boost::exit_success), unspecified;
      }),

      std::make_pair('i', [this](auto&&...) mutable
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

      std::make_pair('l', [this](const object& s) mutable
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
      std::make_pair("batch", [this](auto&&...) mutable
      {
        return batch_mode = t;
      }),

      std::make_pair("debug", [this](auto&&...) mutable
      {
        return debug_mode = t;
      }),

      std::make_pair("help", [this](auto&&...)
      {
        display_help();
        return std::exit(boost::exit_success), unspecified;
      }),

      std::make_pair("interactive", [this](auto&&...) mutable
      {
        return interactive_mode = t;
      }),

      // TODO --srfi=0,1,2
      // TODO --reviced=4,5,7

      std::make_pair("trace", [this](auto&&...) mutable
      {
        return trace_mode = t;
      }),

      std::make_pair("verbose", [this](auto&&...) mutable
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
      std::make_pair("echo", [this](const auto& xs)
      {
        writeln(xs);
        return unspecified;
      }),

      std::make_pair("evaluate", [&](auto&&... xs)
      {
        std::cout << evaluate(std::forward<decltype(xs)>(xs)...) << std::endl;
        return unspecified;
      }),

      std::make_pair("load", [this](const object& s) mutable
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

      std::make_pair("variable", [this](const auto& xs) mutable
      {
        std::cerr << "; configure\t; " << variable << " => " << (variable = xs) << std::endl;
        return variable;
      }),
    };

  public: // Command Line Parser
    template <typename... Ts>
    constexpr decltype(auto) configure(Ts&&... xs)
    {
      #if __cpp_lib_invoke
      return std::invoke(*this, std::forward<decltype(xs)>(xs)...);
      #else
      return (*this)(std::forward<decltype(xs)>(xs)...);
      #endif
    }

    decltype(auto) operator()(const int argc, char const* const* const argv)
    {
      const std::vector<std::string> options {argv + 1, argv + argc};

      #if __cpp_lib_invoke
      return std::invoke(*this, options);
      #else
      return (*this)(options);
      #endif
    }

    void operator()(const std::vector<std::string>& args)
    {
      static const std::regex pattern {"--([[:alnum:]][-_[:alnum:]]+)(=(.*))?|-([[:alnum:]]+)"};

      #if __cpp_lib_nonmember_container_access
      if (std::empty(args))
      #else
      if (args.empty())
      #endif
      {
        interactive_mode = t;
      }

      for (auto option {std::begin(args)}; option != std::end(args); ++option) [&]()
      {
        std::smatch analysis {};
        std::regex_match(*option, analysis, pattern);

        // std::cerr << ";\t\t; analysis[0] " << analysis[0] << std::endl;
        // std::cerr << ";\t\t; analysis[1] " << analysis[1] << std::endl;
        // std::cerr << ";\t\t; analysis[2] " << analysis[2] << std::endl;
        // std::cerr << ";\t\t; analysis[3] " << analysis[3] << std::endl;
        // std::cerr << ";\t\t; analysis[4] " << analysis[4] << std::endl;

        if (const auto sos {analysis.str(4)}; sos.length()) // short-options
        {
          for (auto so {std::begin(sos)}; so != std::end(sos); ++so) // each short-option
          {
            if (auto callee {short_options_.find(*so)}; callee != std::end(short_options_))
            {
              if (const std::string rest {std::next(so), std::end(sos)}; rest.length())
              {
                const auto operands {static_cast<SK&>(*this).read(rest)};

                #if __cpp_lib_invoke
                return std::invoke(cdr(*callee), operands);
                #else
                return cdr(*callee)(operands);
                #endif
              }
              else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
              {
                const auto operands {static_cast<SK&>(*this).read(*option)};

                #if __cpp_lib_invoke
                return std::invoke(cdr(*callee), operands);
                #else
                return cdr(*callee)(operands);
                #endif
              }
              else
              {
                throw configuration_error {*so, " requires operands"};
              }
            }
            else if (auto callee {short_options.find(*so)}; callee != std::end(short_options))
            {
              #if __cpp_lib_invoke
              return std::invoke(cdr(*callee), unit);
              #else
              return cdr(*callee)(unit);
              #endif
            }
            else
            {
              throw configuration_error {*so, " is unknown short-option (in ", *option, ")"};
            }
          }
        }
        else if (const auto lo {analysis.str(1)}; lo.length())
        {
          if (auto callee {long_options_.find(lo)}; callee != std::end(long_options_))
          {
            if (analysis.length(2)) // argument part
            {
              const auto operands {static_cast<SK&>(*this).read(analysis.str(3))};

              #if __cpp_lib_invoke
              return std::invoke(cdr(*callee), operands);
              #else
              return cdr(*callee)(operands);
              #endif
            }
            else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
            {
              const auto operands {static_cast<SK&>(*this).read(*option)};

              #if __cpp_lib_invoke
              return std::invoke(cdr(*callee), operands);
              #else
              return cdr(*callee)(operands);
              #endif
            }
            else
            {
              throw configuration_error {lo, " requires operands"};
            }
          }
          else if (auto callee {long_options.find(lo)}; callee != std::end(long_options))
          {
            #if __cpp_lib_invoke
            return std::invoke(cdr(*callee), unit);
            #else
            return cdr(*callee)(unit);
            #endif
          }
          else
          {
            throw configuration_error {*option, " is unknown long-option"};
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
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
