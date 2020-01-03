#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>

#include <boost/cstdlib.hpp>

#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/version.hpp>

namespace meevax::kernel
{
  template <typename SyntacticContinuation>
  struct configurator
  {
    static inline const auto install_prefix {make<path>("/usr/local")};

    static inline object preloads {unit};

    static inline const version version_object;
    static inline const feature feature_object;

    static inline auto trace    {false_object};
    static inline auto variable {unit};
    static inline auto verbose  {false_object};

  public:
    template <typename T>
    using dispatcher = std::unordered_map<T, std::function<PROCEDURE()>>;

    const dispatcher<char> short_options
    {
      std::make_pair('h', [&](const auto&, const auto&)
      {
        std::cout << "; help!" << std::endl;
        return std::exit(boost::exit_success), unspecified;
      }),

      std::make_pair('v', [&](const auto&, const auto&)
      {
        std::cout << version_object << std::endl;
        return std::exit(boost::exit_success), unspecified;
      }),
    };

    const dispatcher<char> short_options_
    {
    };

    const dispatcher<std::string> long_options
    {
      std::make_pair("help", [&](const auto&, const auto&)
      {
        std::cout << "; help!" << std::endl;
        return std::exit(boost::exit_success), unspecified;
      }),

      std::make_pair("trace", [&](auto&&...) mutable
      {
        return trace = true_object;
      }),

      std::make_pair("verbose", [&](auto&&...) mutable
      {
        return verbose = true_object;
      }),

      std::make_pair("version", [&](const auto&, const auto&)
      {
        std::cout << "; Meevax Lisp System " << version_object.major
                  << " - Revision " << version_object.minor
                  << " Patch " << version_object.patch
                  << std::endl;

        std::cout << ";" << std::endl;
        std::cout << "; version\t\t; " << version_object << std::endl;
        std::cout << "; feature\t\t; " << feature_object << std::endl;

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

      std::make_pair("variable", [&](const auto&, const auto& operands) mutable
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
          const auto filename {make<path>(*option)};
          std::cerr << "; configure\t; file " << filename << std::endl;
        }

        return unspecified;
      }();
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

