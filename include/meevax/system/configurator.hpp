#ifndef INCLUDED_MEEVAX_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_CONFIGURATOR_HPP

#include <regex>
#include <unordered_map>
#include <vector>

#include <boost/cstdlib.hpp>

#include <meevax/system/list.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/path.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/reader.hpp>

namespace meevax::system
{
  template <typename Environment>
  struct configurator
  {
    static inline const auto version_major {make<real>(0)};
    static inline const auto version_minor {make<real>(1)};
    static inline const auto version_patch {make<real>(706)};

    static inline const auto version {list(
      version_major, version_minor, version_patch
    )};

    // static inline const struct version_container
    //   : public object
    // {
    //   inline const auto major {make<real>(0)};
    //   inline const auto minor {make<real>(1)};
    //   inline const auto patch {make<real>(706)};
    //
    //   explicit version_container()
    //     : object {list(major, minor, patch)}
    //   {}
    // } version {};

    // static inline const std::string build_date {"2019/09/12 12:59:43"};
    static inline const auto build_date {datum<string>("2019/09/12 12:59:43")};

    static inline const std::string build_hash {"b0fcd72dfe6eba0b536f2d0881169ea36e33ecf8"};
    static inline const std::string build_type {"debug"};

    static inline const auto install_prefix {make<path>("/usr/local")};

    static inline object preloads {unit};

    static inline auto debug   {false_object};
    static inline auto verbose {false_object};

  public:
    template <typename T>
    using dispatcher = std::unordered_map<
                         typename std::decay<T>::type,
                         std::function<PROCEDURE()>
                       >;

    static inline const std::unordered_map<
                          char,
                          std::function<PROCEDURE()>
                        > short_options_requires_no_operands
    {
      std::make_pair('h', [&](const auto&)
      {
        std::cout << "HELP!" << std::endl;
        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),

      std::make_pair('v', [&](const auto&)
      {
        std::cout << version << std::endl;
        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),
    };

    static inline std::unordered_map<
                    char,
                    std::function<PROCEDURE()>
                  > short_options_requires_operands
    {
    };

    static inline std::unordered_map<
                    std::string,
                    std::function<PROCEDURE()>
                  > long_options_requires_no_operands
    {
      std::make_pair("help", [&](const auto&)
      {
        std::cout << "; HELP!" << std::endl;
        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),

      std::make_pair("version", [&](const auto&)
      {
        std::cout << "; Meevax Lisp System " << version_major << " - Revision " << version_minor << " Patch " << version_patch << std::endl;
        std::cout << ";" << std::endl;
        std::cout << "; version   \t; " << version    << std::endl;
        std::cout << "; build-date\t; " << build_date << std::endl;
        std::cout << "; build-hash\t; " << build_hash << std::endl;
        std::cout << "; build-type\t; " << build_type << std::endl;
        std::cout << ";" << std::endl;
        std::cout << "; install-prefix\t; " << install_prefix << std::endl;

        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),
    };

    static inline const std::unordered_map<
                          std::string,
                          std::function<PROCEDURE()>
                        > long_options_requires_operands
    {
      std::make_pair("echo", [&](const auto& operands)
      {
        std::cout << operands << std::endl;
        return undefined;
      }),

      std::make_pair("verbose", [&](const auto& operands)
      {
        // TODO Typecheck
        // TODO Accumulate operands with std::logical_and
        return verbose = car(operands);
      }),
    };

    template <typename... Ts>
    constexpr decltype(auto) configure(Ts&&... xs)
    {
      return (*this)(std::forward<Ts>(xs)...);
    }

    decltype(auto) operator()(const int argc, char const* const* const argv)
    {
      const std::vector<std::string> options {argv + 1, argv + argc};
      return (*this)(options);
    }

    void operator()(const std::vector<std::string>& args)
    {
      static const std::regex pattern {"--([[:alnum:]][-_[:alnum:]]+)(=(.*))?|-([[:alnum:]]+)"};

      for (auto global {std::begin(args)}; global != std::end(args); ++global) [&]()
      {
        std::cerr << ";" << std::endl
                  << "; configure\t; " << *global << std::endl;

        std::smatch group {};
        std::regex_match(*global, group, pattern);

        std::cerr << ";\t\t; group[0] " << group[0] << std::endl;
        std::cerr << ";\t\t; group[1] " << group[1] << std::endl;
        std::cerr << ";\t\t; group[2] " << group[2] << std::endl;
        std::cerr << ";\t\t; group[3] " << group[3] << std::endl;
        std::cerr << ";\t\t; group[4] " << group[4] << std::endl;

        if (group[4].length() != 0) // short-option
        {
          const auto buffer {group.str(4)};
          std::cerr << ";\t\t; search short-options " << buffer << std::endl;

          for (auto local {std::begin(buffer)}; local != std::end(buffer); ++local)
          {
            std::cerr << ";\t\t; search short-option " << *local << std::endl;

            if (auto callee_requires_operands {short_options_requires_operands.find(*local)};
                callee_requires_operands != std::end(short_options_requires_operands))
            {
              std::cerr << ";\t\t; found short-option " << *local << " (requires operands)" << std::endl;

              if (const std::string subsequent {std::next(local), std::end(buffer)}; not subsequent.empty())
              {
                std::cerr << ";\t\t; operand(s) " << subsequent << std::endl;
                return std::invoke(std::get<1>(*callee_requires_operands), static_cast<Environment&>(*this).read(subsequent));
              }
              else if (std::smatch next_group {};
                       std::next(global) != std::end(args)
                       and not std::regex_match(*std::next(global), next_group, pattern))
              {
                std::cerr << ";\t\t; operand(s) " << *std::next(global) << std::endl;
                return std::invoke(std::get<1>(*callee_requires_operands), static_cast<Environment&>(*this).read(*++global));
              }
              else
              {
                throw error {*local, " requires operands"};
              }
            }
            else if (auto callee_requires_no_operands {short_options_requires_no_operands.find(*local)};
                     callee_requires_no_operands != std::end(short_options_requires_no_operands))
            {
              return std::invoke(std::get<1>(*callee_requires_no_operands), unit);
            }
            else
            {
              // TODO config-error
              throw error {*local, " is unknown short-option (in ", *global, ")"};
            }
          }
        }
        else if (group[1].length() != 0) // long option
        {
          const auto buffer {group.str(1)};
          std::cerr << ";\t\t; search long-option " << buffer << std::endl;

          if (auto callee_requires_operands {long_options_requires_operands.find(buffer)};
              callee_requires_operands != std::end(long_options_requires_operands))
          {
            std::cerr << ";\t\t; found long-option " << buffer << " (requires operands)" << std::endl;

            if (group.length(2) != 0)
            {
              std::cerr << ";\t\t; operand(s) " << group.str(3) << std::endl;
            }
            else if (std::smatch next_group {};
                     std::next(global) != std::end(args)
                     and not std::regex_match(*std::next(global), next_group, pattern))
            {
              std::cerr << ";\t\t; operand(s) " << *std::next(global) << std::endl;
              return std::invoke(std::get<1>(*callee_requires_operands), static_cast<Environment&>(*this).read(*++global));
            }
            else
            {
              throw error {buffer, " requires operands"};
            }
          }
          else if (auto callee_requires_no_operands {long_options_requires_no_operands.find(buffer)};
                   callee_requires_no_operands != std::end(long_options_requires_no_operands))
          {
            std::cerr << ";\t\t; found long-option " << buffer << " (requires no operands)" << std::endl;
            return std::invoke(std::get<1>(*callee_requires_no_operands), unit);
          }
          else
          {
            throw error {*global, " is unknown long-option"};
          }
        }
        else
        {
          const auto filename {make<path>(*global)};
          std::cerr << ";\t\t; append " << filename << " to preloads" << std::endl;
          preloads = append(preloads, filename);
          std::cerr << ";\t\t; preloads " << preloads << std::endl;
        }

        return undefined;
      }();
    }
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURATOR_HPP

