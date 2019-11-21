#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <regex>
#include <vector>

#include <boost/cstdlib.hpp>

#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/reader.hpp>

// XXX DIRTY HACK
#include <meevax/posix/linker.hpp>

namespace meevax::kernel
{
  template <typename Environment>
  struct configurator
  {
    /* ==== Version Informations ==============================================
    *
    * This structure is a precaution against the possibility of providing part
    * of the configuration as an object to the runtime in the future.
    *
    *======================================================================= */
    static inline const struct version
      : public object
    {
      static inline const auto major {make<real>(${PROJECT_VERSION_MAJOR})};
      static inline const auto minor {make<real>(${PROJECT_VERSION_MINOR})};
      static inline const auto patch {make<real>(${PROJECT_VERSION_PATCH})};

      static inline const auto semantic {"datum<string>(${PROJECT_VERSION})"};

      template <typename... Ts>
      explicit constexpr version(Ts&&... operands)
        : object {list(std::forward<decltype(operands)>(operands)...)}
      {}
    } version {
      version::major,
      version::minor,
      version::patch,
    };

    /* ==== Build Informations ================================================
    *
    * This structure is a precaution against the possibility of providing part
    * of the configuration as an object to the runtime in the future.
    *
    *======================================================================= */
    static inline const struct build // TODO Rename
    {
      static inline const auto date {datum<string>("${${PROJECT_NAME}_BUILD_DATE}")};
      static inline const auto hash {datum<string>("${${PROJECT_NAME}_BUILD_HASH}")};
      static inline const auto type {datum<string>("${CMAKE_BUILD_TYPE}")};
    } build {};

    static inline const auto install_prefix {make<path>("${CMAKE_INSTALL_PREFIX}")};

    static inline object preloads {unit};

    static inline auto debug               {false_object};
    static inline auto experimental        {false_object};
    static inline auto trace               {false_object};
    static inline auto variable            {unit};
    static inline auto verbose             {false_object};
    static inline auto verbose_compiler    {false_object};
    static inline auto verbose_define      {false_object}; // TODO Rename to "verbose_syntax"
    static inline auto verbose_environment {false_object};
    static inline auto verbose_loader      {false_object};
    static inline auto verbose_machine     {false_object};
    static inline auto verbose_reader      {false_object};

    #define ENABLE(VARIABLE)                                                   \
    [&](const auto&) mutable                                                   \
    {                                                                          \
      std::cerr << ";\t\t; " << VARIABLE << " => ";                            \
      VARIABLE = true_object;                                                  \
      std::cerr << VARIABLE << std::endl;                                      \
      return VARIABLE;                                                         \
    }

  public:
    /**
     * TODO
     * Simplify the parser by adding a default constructor that constructs a
     * procedure that either returns a NIL for a procedure type or throws an
     * exception, meaning that it is undecided.
     **/
    template <typename T>
    using dispatcher = std::unordered_map<
                         typename std::decay<T>::type,
                         std::function<PROCEDURE()>
                       >;

    const dispatcher<char> short_options_requires_no_operands
    {
      std::make_pair('h', [&](const auto&)
      {
        std::cout << "; rather, help me." << std::endl;
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

    const dispatcher<char> short_options_requires_operands
    {
    };

    const dispatcher<std::string> long_options_requires_no_operands
    {
      std::make_pair("debug", ENABLE(debug)),

      std::make_pair("experimental", ENABLE(experimental)),

      std::make_pair("help", [&](const auto&)
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

      std::make_pair("version", [&](const auto&)
      {
        std::cout << "; Meevax Lisp System " << version.major << " - Revision " << version.minor << " Patch " << version.patch << std::endl;
        std::cout << ";" << std::endl;
        std::cout << "; version   \t; " << version    << std::endl;
        std::cout << "; build-date\t; " << build.date << std::endl;
        std::cout << "; build-hash\t; " << build.hash << std::endl;
        std::cout << "; build-type\t; " << build.type << std::endl;
        std::cout << ";" << std::endl;
        std::cout << "; install-prefix\t; " << install_prefix << std::endl;

        std::exit(boost::exit_success);
        return undefined; // dummy for return type deduction
      }),
    };

    const dispatcher<std::string> long_options_requires_operands
    {
      std::make_pair("echo", [](const auto& operands)
      {
        std::cout << operands << std::endl;
        return undefined;
      }),

      std::make_pair("debug-variable", [&](const auto& operands) mutable
      {
        std::cerr << "; configure\t; " << verbose << " => ";
        variable = operands;
        std::cerr << variable << std::endl;
        return variable;
      }),
    };

    template <typename... Ts>
    constexpr decltype(auto) configure(Ts&&... operands)
    {
      return (*this)(std::forward<decltype(operands)>(operands)...);
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
                const auto operands {static_cast<Environment&>(*this).read(subsequent)};
                std::cerr << ";\t\t; operand(s) " << operands << std::endl;
                return std::invoke(std::get<1>(*callee_requires_operands), operands);
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
                throw configuration_error {*local, " requires operands"};
              }
            }
            else if (auto callee_requires_no_operands {short_options_requires_no_operands.find(*local)};
                     callee_requires_no_operands != std::end(short_options_requires_no_operands))
            {
              return std::invoke(std::get<1>(*callee_requires_no_operands), unit);
            }
            else
            {
              throw configuration_error {*local, " is unknown short-option (in ", *global, ")"};
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
              std::cerr << ";\t\t; operand(s) \"" << group.str(3) << "\" => ";
              const auto operands {static_cast<Environment&>(*this).read(group.str(3))};
              std::cerr << operands << std::endl;
              return std::invoke(std::get<1>(*callee_requires_operands), operands);
            }
            else if (std::smatch next_group {};
                     std::next(global) != std::end(args)
                     and not std::regex_match(*std::next(global), next_group, pattern))
            {
              std::cerr << ";\t\t; operand(s) \"" << *std::next(global) << "\" => ";
              const auto operands {static_cast<Environment&>(*this).read(*++global)};
              std::cerr << operands << std::endl;
              return std::invoke(std::get<1>(*callee_requires_operands), operands);
            }
            else
            {
              throw configuration_error {buffer, " requires operands"};
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
            throw configuration_error {*global, " is unknown long-option"};
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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

