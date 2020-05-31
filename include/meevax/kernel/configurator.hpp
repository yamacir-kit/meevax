#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>

#include <boost/cstdlib.hpp>

#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class configurator
  {
    friend SK;

    explicit configurator()
    {}

    Import_Const(SK, current_verbose_port);
    Import_Const(SK, write);
    Import_Const(SK, write_to);

    object debug_mode       { f };
    object interactive_mode { f };
    object quiet_mode       { f };
    object trace_mode       { f };
    object verbose_mode     { f };

  public:
    static inline const version current_version {};
    static inline const feature current_feature {};

    object paths    { unit };
    object variable { unit };

    auto debugging() const
    {
      return debug_mode.as<boolean>().value;
    }

    auto interactive() const
    {
      return interactive_mode.as<boolean>().value;
    }

    auto quiet() const
    {
      return quiet_mode.as<boolean>().value;
    }

    auto tracing() const
    {
      return trace_mode.as<boolean>().value;
    }

    auto verbose() const
    {
      return verbose_mode.as<boolean>().value;
    }

  public:
    void display_title(const version& v) const
    {
      write( //  10        20        30        40        50        60        70        80\n"
        "; Meevax Lisp System ", v.major(), " - Revision ", v.minor(), " Patch ", v.patch(), "\n"
        ";                                                                               \n");
    }

    void display_abstract() const
    {
      write( //  10        20        30        40        50        60        70        80\n"
        "; Abstract:                                                                     \n"
        ";   ICE is Incremental Compiler/Evaluator of Lisp-1 programming language Meevax.\n"
        ";                                                                               \n");
    }

    auto display_version() const -> const auto&
    {
      display_title(current_version);

      write( //  10        20        30        40        50        60        70        80\n"
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

    auto display_help() const -> const auto&
    {
      #define SECTION_HEADING(NAME) \
        "; ", console::bold, NAME "\n", console::reset,

      #define SUBSECTION_HEADING(NAME) \
        ";    ", console::bold, NAME "\n", console::reset,

      write(
      // ;       10        20        30        40        50        60        70        80\n"
        ";                 Meevax Lisp System ", current_version.major(), " - Revision ", current_version.minor(), " Patch ", current_version.patch(), "\n"
        ";\n"
        SECTION_HEADING("NAME")
        ";        ice - Meevax incremental compiler/evaluator.\n"
        ";\n"
        SECTION_HEADING("SYNOPSIS")
        ";        ice [option]... [path]...\n"
        ";\n"
        SECTION_HEADING("DESCRIPTION")
        ";\n"
        SECTION_HEADING("OPTIONS")
          SUBSECTION_HEADING("Generic Program Information")
        ";        -h, --help\n"
        ";               Display version information and exit.\n"
        ";\n"
        ";        -v, --version\n"
        ";               Display this help message and exit.\n"
        ";\n"
          SUBSECTION_HEADING("Operation Mode")
        ";        -i, --interactive\n"
        ";               Take over the control of root syntactic continuation\n"
        ";               interactively after processing given <file>s.\n"
        ";\n"
        ";        -q, --quiet\n"
        ";               Suppress any output except side-effect of user's explicit use of\n"
        ";               primitive procedure 'write' or 'display'.\n"
        ";\n"
        ";       --trace Display stacks of virtual machine on each execution step.\n"
        ";\n"
        ";       --verbose\n"
        ";               Report the details of lexical parsing, compilation, virtual\n"
        ";               machine execution to standard-error-port.\n"
        ";\n"
          SUBSECTION_HEADING("Evaluation Target")
        ";        -f path, --file path\n"
        ";               Specify the file to be executed. If this option is used multiple\n"
        ";               times, the specified files will be executed sequentially from\n"
        ";               left to right. Anything that is not an option name or option\n"
        ";               argument is implicitly treated as an argument for this option.\n"
        ";\n"
          SUBSECTION_HEADING("Tools")
        ";        --echo expression\n"
        ";               Read an expression, construct an object from it, and display its\n"
        ";               external representation. Note that the expression is parsed once\n"
        ";               by the shell before it is read. This output is useful to see\n"
        ";               what objects the --evaluate option accepts.\n"
        ";\n"
        ";        -e expression, --evaluate expression\n"
        ";               Read an expression, construct an object from it, compile and\n"
        ";               execute it, and then display external representation of the\n"
        ";               result.\n"
        ";\n"
        SECTION_HEADING("AUTHORS")
        ";        Tatsuya Yamasaki\n"
        ";\n"
        SECTION_HEADING("LICENSE")
        ";        ", unspecified, "\n"
        );

      return unspecified;
    }

  public:
    template <typename T>
    using dispatcher = std::unordered_map<T, std::function<PROCEDURE()>>;

    // NOTE
    //   --from=FILE --to=FILE
    //   --input=FILE --output=FILE

    const dispatcher<char> short_options
    {
      std::make_pair('d', [this](auto&&...) mutable
      {
        return static_cast<SK&>(*this).debug_mode = t;
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

      std::make_pair('q', [this](auto&&...) mutable
      {
        return static_cast<SK&>(*this).quiet_mode = t;
      }),

      std::make_pair('v', [this](auto&&...)
      {
        display_version();
        return std::exit(boost::exit_success), unspecified;
      }),
    };

    const dispatcher<char> short_options_
    {
      std::make_pair('e', [this](auto&&, auto&& operands)
      {
        std::cout << static_cast<SK&>(*this).evaluate(
                       std::forward<decltype(operands)>(operands))
                  << std::endl;
        return unspecified;
      }),

      std::make_pair('f', [this](auto&&, const object& s) mutable
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
      std::make_pair("debug", [this](auto&&...) mutable
      {
        return static_cast<SK&>(*this).debug_mode = t;
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

      std::make_pair("quiet", [this](auto&&...) mutable
      {
        return static_cast<SK&>(*this).quiet_mode = t;
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
      std::make_pair("echo", [](const auto&, const auto& operands)
      {
        std::cout << operands << std::endl;
        return unspecified;
      }),

      std::make_pair("evaluate", [this](auto&&, auto&& operands)
      {
        std::cout << static_cast<SK&>(*this).evaluate(
                       std::forward<decltype(operands)>(operands))
                  << std::endl;
        return unspecified;
      }),

      std::make_pair("file", [this](auto&&, const object& s) mutable
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

      std::make_pair("variable", [this](const auto&, const auto& operands) mutable
      {
        std::cerr << "; configure\t; "
                  << variable << " => " << (variable = operands)
                  << std::endl;
        return variable;
      }),
    };

  public: // Command Line Parser
    template <typename... Ts>
    constexpr decltype(auto) configure(Ts&&... operands)
    {
      return std::invoke(*this, std::forward<decltype(operands)>(operands)...);
    }

    decltype(auto) operator()(const int argc, char const* const* const argv)
    {
      const std::vector<std::string> options {argv + 1, argv + argc};
      return std::invoke(*this, options);
    }

    void operator()(const std::vector<std::string>& args)
    {
      static const std::regex pattern {"--([[:alnum:]][-_[:alnum:]]+)(=(.*))?|-([[:alnum:]]+)"};

      if (std::empty(args))
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
                return std::invoke(std::get<1>(*callee), resource {}, operands);
              }
              else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
              {
                const auto operands {static_cast<SK&>(*this).read(*option)};
                return std::invoke(std::get<1>(*callee), resource {}, operands);
              }
              else
              {
                throw configuration_error {*so, " requires operands"};
              }
            }
            else if (auto callee {short_options.find(*so)}; callee != std::end(short_options))
            {
              return std::invoke( std::get<1>(*callee), resource {}, unit);
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
              return std::invoke(std::get<1>(*callee), resource {}, operands);
            }
            else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
            {
              const auto operands {static_cast<SK&>(*this).read(*option)};
              return std::invoke(std::get<1>(*callee), resource {}, operands);
            }
            else
            {
              throw configuration_error {lo, " requires operands"};
            }
          }
          else if (auto callee {long_options.find(lo)}; callee != std::end(long_options))
          {
            return std::invoke(std::get<1>(*callee), resource {}, unit);
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

      if (interactive() and std::experimental::filesystem::exists(rc))
      {
        paths = cons(make<path>(rc), paths);
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

