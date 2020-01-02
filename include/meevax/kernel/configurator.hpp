#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>
#include <vector>

#include <boost/cstdlib.hpp>

#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/version.hpp>

// XXX DIRTY HACK
#include <meevax/posix/linker.hpp>

namespace meevax::kernel
{

  template <typename SyntacticContinuation>
  struct configurator
  {
    static inline const auto install_prefix {make<path>("/usr/local")};

    static inline object preloads {unit};

    static inline const version version_object;
    static inline const feature feature_object;

    static inline auto debug               {false_object};
    static inline auto rune_magic          {false_object};
    static inline auto trace               {false_object};
    static inline auto variable            {unit};
    static inline auto verbose             {false_object};
    static inline auto verbose_compiler    {false_object};
    static inline auto verbose_define      {false_object}; // TODO Rename to "verbose_syntax"
    static inline auto verbose_environment {false_object};
    static inline auto verbose_loader      {false_object};
    static inline auto verbose_machine     {false_object};
    static inline auto verbose_reader      {false_object};

  public:
    template <typename T>
    using dispatcher
      = std::unordered_map<
          typename std::decay<T>::type,
          std::function<PROCEDURE()>
        >;

    #define ENABLE(VARIABLE)                                                   \
    [&](const auto&, const auto&) mutable                                      \
    {                                                                          \
      std::cerr << ";\t\t; " << VARIABLE << " => ";                            \
      VARIABLE = true_object;                                                  \
      std::cerr << VARIABLE << std::endl;                                      \
      return VARIABLE;                                                         \
    }

    const dispatcher<char> short_options_requires_no_operands
    {
      std::make_pair('h', [&](const auto&, const auto&)
      {
        std::cout << "; HELP!" << std::endl;
        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),

      std::make_pair('v', [&](const auto&, const auto&)
      {
        std::cout << version_object << std::endl;
        std::exit(boost::exit_success);
        return unspecified;
      }),
    };

    const dispatcher<char> short_options_requires_operands
    {
    };

    const dispatcher<std::string> long_options_requires_no_operands
    {
      std::make_pair("debug", ENABLE(debug)),

      std::make_pair("rune-magic", ENABLE(rune_magic)),

      std::make_pair("help", [&](const auto&, const auto&)
      {
        std::cout << "; rather, help me." << std::endl;
        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),

      // TODO quite

      std::make_pair("trace", ENABLE(trace)),

      std::make_pair("verbose",             ENABLE(verbose)),
      std::make_pair("verbose-compiler",    ENABLE(verbose_compiler)),
      std::make_pair("verbose-define",      ENABLE(verbose_define)),
      std::make_pair("verbose-environment", ENABLE(verbose_environment)),
      std::make_pair("verbose-linker",      ENABLE(posix::verbose_linker)),
      std::make_pair("verbose-loader",      ENABLE(verbose_loader)),
      std::make_pair("verbose-machine",     ENABLE(verbose_machine)),
      std::make_pair("verbose-reader",      ENABLE(verbose_reader)),

      std::make_pair("version", [&](const auto&, const auto&)
      {
        std::cout << "; Meevax Lisp System "
                  << version_object.major
                  << " - Revision "
                  << version_object.minor
                  << " Patch "
                  << version_object.patch
                  << std::endl;

        std::cout << ";" << std::endl;
        std::cout << "; version\t\t; " << version_object << std::endl;
        std::cout << "; feature\t\t; " << feature_object << std::endl;

        std::exit(boost::exit_success);
        return unspecified;
      }),
    };

    const dispatcher<std::string> long_options_requires_operands
    {
      std::make_pair("echo", [](const auto&, const auto& operands)
      {
        std::cout << operands << std::endl;
        return undefined;
      }),

      std::make_pair("debug-variable", [&](const auto&, const auto& operands) mutable
      {
        std::cerr << "; configure\t; " << verbose << " => ";
        variable = operands;
        std::cerr << variable << std::endl;
        return variable;
      }),
    };

    #undef ENABLE

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

      for (auto global {std::begin(args)}; global != std::end(args); ++global) [&]()
      {
        // std::cerr << "; configure\t; " << *global << std::endl;

        std::smatch analysis {};
        std::regex_match(*global, analysis, pattern);

        // std::cerr << ";\t\t; analysis[0] " << analysis[0] << std::endl;
        // std::cerr << ";\t\t; analysis[1] " << analysis[1] << std::endl;
        // std::cerr << ";\t\t; analysis[2] " << analysis[2] << std::endl;
        // std::cerr << ";\t\t; analysis[3] " << analysis[3] << std::endl;
        // std::cerr << ";\t\t; analysis[4] " << analysis[4] << std::endl;

        if (analysis[4].length()) // short-option
        {
          const auto buffer {analysis.str(4)};
          // std::cerr << ";\t\t; search short-options " << buffer << std::endl;

          for (auto local {std::begin(buffer)}; local != std::end(buffer); ++local)
          {
            // std::cerr << ";\t\t; search short-option " << *local << std::endl;

            if (auto callee_requires_operands {short_options_requires_operands.find(*local)};
                callee_requires_operands != std::end(short_options_requires_operands))
            {
              // std::cerr << ";\t\t; found short-option " << *local << " (requires operands)" << std::endl;

              if (const std::string subsequent {std::next(local), std::end(buffer)}; not subsequent.empty())
              {
                const auto operands {static_cast<SyntacticContinuation&>(*this).read(subsequent)};
                // std::cerr << ";\t\t; operand(s) " << operands << std::endl;
                return std::invoke(std::get<1>(*callee_requires_operands), resource {}, operands);
              }
              else if (std::smatch next_analysis {};
                       std::next(global) != std::end(args)
                       and not std::regex_match(*std::next(global), next_analysis, pattern))
              {
                const auto operands {static_cast<SyntacticContinuation&>(*this).read(*++global)};
                // std::cerr << ";\t\t; operand(s) " << *std::next(global) << std::endl;
                return std::invoke(std::get<1>(*callee_requires_operands), resource {}, operands);
              }
              else
              {
                throw configuration_error {*local, " requires operands"};
              }
            }
            else if (auto callee_requires_no_operands {short_options_requires_no_operands.find(*local)};
                     callee_requires_no_operands != std::end(short_options_requires_no_operands))
            {
              return std::invoke( std::get<1>(*callee_requires_no_operands), resource {}, unit);
            }
            else
            {
              throw configuration_error {*local, " is unknown short-option (in ", *global, ")"};
            }
          }
        }
        else if (analysis[1].length()) // long option
        {
          const auto buffer {analysis.str(1)};
          // std::cerr << ";\t\t; search long-option " << buffer << std::endl;

          if (auto callee_requires_operands {long_options_requires_operands.find(buffer)};
              callee_requires_operands != std::end(long_options_requires_operands))
          {
            // std::cerr << ";\t\t; found long-option " << buffer << " (requires operands)" << std::endl;

            if (analysis.length(2))
            {
              // std::cerr << ";\t\t; operand(s) \"" << analysis.str(3) << "\" => ";
              const auto operands {static_cast<SyntacticContinuation&>(*this).read(analysis.str(3))};
              // std::cerr << operands << std::endl;
              return std::invoke(std::get<1>(*callee_requires_operands), resource {}, operands);
            }
            else if (std::smatch next_analysis {};
                     std::next(global) != std::end(args)
                     and not std::regex_match(*std::next(global), next_analysis, pattern))
            {
              // std::cerr << ";\t\t; operand(s) \"" << *std::next(global) << "\" => ";
              const auto operands {static_cast<SyntacticContinuation&>(*this).read(*++global)};
              // std::cerr << operands << std::endl;
              return std::invoke(std::get<1>(*callee_requires_operands), resource {}, operands);
            }
            else
            {
              throw configuration_error {buffer, " requires operands"};
            }
          }
          else if (auto callee_requires_no_operands {long_options_requires_no_operands.find(buffer)};
                   callee_requires_no_operands != std::end(long_options_requires_no_operands))
          {
            // std::cerr << ";\t\t; found long-option " << buffer << " (requires no operands)" << std::endl;
            return std::invoke(std::get<1>(*callee_requires_no_operands), resource {}, unit);
          }
          else
          {
            throw configuration_error {*global, " is unknown long-option"};
          }
        }
        else
        {
          const auto filename {make<path>(*global)};
          // std::cerr << ";\t\t; append " << filename << " to preloads" << std::endl;
          preloads = append(preloads, filename);
          // std::cerr << ";\t\t; preloads " << preloads << std::endl;
        }

        return unspecified;
      }();
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

