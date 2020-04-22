#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>

#include <boost/cstdlib.hpp>

#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax::kernel
{
  template <typename SyntacticContinuation>
  struct configurator
  {
    // static inline const auto install_prefix {make<path>("/usr/local")};

    static inline const version version_object {};
    static inline const feature feature_object {};

    // TODO Generate from CMakeLists.txt
    // static inline const std::string program_name {"ice"};

    object interactive {f};
    object quiet       {f};
    object trace       {f};
    object verbose     {f};

    object ports       {unit};
    object variable    {unit};

    /* =========================================================================
    *
    * XXX explicit configurator() = default;
    *     causes segmentation fault on access above variables.
    *
    *======================================================================== */
    explicit configurator()
    {}

  public:
    static void display_title(const version& v)
    {           // ";       10        20        30        40        50        60        70        80\n"
      std::cout << "; Meevax Lisp System " << v.major << " - Revision " << v.minor << " Patch " << v.patch << "\n"
                   ";\n";
    }

    static void display_abstract()
    {           // ";       10        20        30        40        50        60        70        80\n"
      std::cout << "; Abstract:\n"
                   ";   ICE is incremental compiler of Lisp-1 programming language Meevax.\n"
                   ";\n";
    }

    static PROCEDURE(display_version)
    {           // "        10        20        30        40        50        60        70        80\n"
      display_title(version_object);

      std::cout << "; version"   "\t; " << version_object.semantic                              << "\n"
                   "; license"   "\t; unspecified (All rights reserved)"                           "\n"
                   ";"                                                                             "\n"
                   "; compiled"  "\t; " << feature_object.date                                  << "\n"
                   "; configuration ; " << feature_object.type                                  << "\n"
                   "; commit"    "\t; " << feature_object.commit                                << "\n"
                   ";"                                                                             "\n"
                   "; feature"   "\t; " << feature_object                                       << "\n";
      return std::exit(boost::exit_success), unspecified;
    }

    static PROCEDURE(display_help)
    {           // "        10        20        30        40        50        60        70        80\n"
      display_title(version_object);

      display_abstract();

      std::cout << "; Usage: ice [option]... [file]..."                                            "\n"
                   ";"                                                                             "\n"
                   "; Operation mode:"                                                             "\n"
                   ";   -i, --interactive         Take over the control of root syntactic           \n"
                   ";                             continuation interactively after processing given \n"
                   ";                             <file>s.                                          \n"
                   ";"                                                                             "\n"
                   "; Tools:"                                                                      "\n"
                   ";       --echo=<expr>         Read an expression, construct an object from it,  \n"
                   ";                             and display its external representation. Note that\n"
                   ";                             the expression is parsed once by the shell before \n"
                   ";                             it is read. This output is useful to see what     \n"
                   ";                             objects the --evaluate option accepts.            \n"
                   ";   -e, --evaluate=<expr>     Read an expression, construct an object from it,  \n"
                   ";                             compile and execute it, and then display external \n"
                   ";                             representation of the result.                     \n"
                   ";"                                                                             "\n"
                   "; Debug:"                                                                      "\n"
                   ";   -q, --quiet               Suppress any output except side-effect of user's  \n"
                   ";                             explicit use of primitive procedure 'write'.      \n"
                   ";       --trace               Display stacks of virtual machine on each         \n"
                   ";                             execution step."                                 "\n"
                   ";       --verbose             Report the details of lexical parsing,            \n"
                   ";                             compilation, virtual machine execution to         \n"
                   ";                             standard-error."                                 "\n"
                   ";"                                                                             "\n"
                   "; Miscellaneous:"                                                              "\n"
                   ";   -h, --help                Display version information and exit."           "\n"
                   ";   -v, --version             Display this help message and exit."             "\n"
                   ";"                                                                             "\n";

      return std::exit(boost::exit_success), unspecified;
    }

  public:
    template <typename T>
    using dispatcher = std::unordered_map<T, std::function<PROCEDURE()>>;

    const dispatcher<char> short_options
    {
      std::make_pair('h', display_help),

      std::make_pair('i', [this](auto&&...) mutable
      {
        std::cout << "; configure\t; interactive mode "
                  << interactive << " => " << (interactive = t)
                  << std::endl;
        return unspecified;
      }),

      std::make_pair('q', [this](auto&&...) mutable
      {
        return quiet = t;
      }),

      std::make_pair('v', display_version),
    };

    const dispatcher<char> short_options_
    {
      std::make_pair('e', [this](auto&&, auto&& operands)
      {
        std::cout << static_cast<SyntacticContinuation&>(*this).evaluate(
                       std::forward<decltype(operands)>(operands))
                  << std::endl;
        return unspecified;
      }),

      std::make_pair('f', [this](auto&&, const object& s) mutable
      {
        if (s.is<symbol>())
        {
          return ports = cons(make<input_port>(s.as<const std::string>()), ports);
        }
        else
        {
          return unspecified;
        }
      }),
    };

    const dispatcher<std::string> long_options
    {
      // TODO --color

      std::make_pair("help", display_help),

      std::make_pair("interactive", [this](auto&&...) mutable
      {
        std::cout << "; configure\t; interactive mode "
                  << interactive << " => " << (interactive = t)
                  << std::endl;
        return unspecified;
      }),

      // TODO DEFINE_LONG_ENABLER

      std::make_pair("quiet", [this](auto&&...) mutable
      {
        return quiet = t;
      }),

      std::make_pair("trace", [this](auto&&...) mutable
      {
        return trace = t;
      }),

      std::make_pair("verbose", [this](auto&&...) mutable
      {
        return verbose = t;
      }),

      std::make_pair("version", display_version),
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
        std::cout << static_cast<SyntacticContinuation&>(*this).evaluate(
                       std::forward<decltype(operands)>(operands))
                  << std::endl;
        return unspecified;
      }),

      std::make_pair("file", [this](auto&&, const object& s) mutable
      {
        if (s.is<symbol>())
        {
          return ports = cons(make<input_port>(s.as<const std::string>()), ports);
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
                const auto operands {static_cast<SyntacticContinuation&>(*this).read(rest)};
                return std::invoke(std::get<1>(*callee), resource {}, operands);
              }
              else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
              {
                const auto operands {static_cast<SyntacticContinuation&>(*this).read(*option)};
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
              const auto operands {static_cast<SyntacticContinuation&>(*this).read(analysis.str(3))};
              return std::invoke(std::get<1>(*callee), resource {}, operands);
            }
            else if (++option != std::end(args) and not std::regex_match(*option, analysis, pattern))
            {
              const auto operands {static_cast<SyntacticContinuation&>(*this).read(*option)};
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
          ports = cons(make<input_port>(*option), ports);
        }

        return unspecified;
      }();

      ports = reverse(ports);
      // std::cerr << "; configure\t; ports are " << ports << std::endl;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

