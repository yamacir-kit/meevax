#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <sstream>

#include <meevax/system/object.hpp>

#include <meevax/utility/requires.hpp>

// - exception
//    |-- error                                                       (category)
//    |    |-- configuration_error                                     (section)
//    |    |-- evaluation_error                                        (section)
//    |    |-- reader_error                                            (section)
//    |    |    |-- reader_error_about_pair                              (about)
//    |    |    `-- reader_error_about_parentheses                       (about)
//    |    |-- syntax_error                                            (section)
//    |    `-- system_error                                            (section)
//    `-- warning                                                     (category)

namespace meevax::system
{
  template <typename Exception, typename... Ts>
  void raise(Ts&&... xs)
  {
    Exception {std::forward<Ts>(xs)...}.raise();
  }

  struct exception
    : public std::runtime_error
  {
    template <typename S, REQUIRES(std::is_constructible<std::string, S>)>
    explicit constexpr exception(S&& s)
      : std::runtime_error {std::forward<S>(s)}
    {}

    template <typename... Objects>
    explicit exception(Objects&&... objects)
      : std::runtime_error {write(
          std::ostringstream {}, std::forward<Objects>(objects)...
        ).str()}
    {}

    virtual ~exception() = default;

    virtual void raise() const
    {
      throw *this;
    }
  };

  #define DEFINE_EXCEPTION_WRITER(TYPENAME, ...)                               \
  auto operator<<(std::ostream& os, const TYPENAME& exception)                 \
    -> decltype(auto)                                                          \
  {                                                                            \
    return os << highlight::syntax << "#("                                     \
              << highlight::constructor << __VA_ARGS__                         \
              << highlight::simple_datum << " " <<  std::quoted(exception.what()) \
              << highlight::syntax << ")"                                      \
              << attribute::normal;                                            \
  }

  DEFINE_EXCEPTION_WRITER(exception, "exception")

  #define DEFINE_EXCEPTION_CATEGORY(CATEGORY)                                  \
  struct [[maybe_unused]] CATEGORY                                             \
    : public exception                                                         \
  {                                                                            \
    template <typename... Ts>                                                  \
    explicit constexpr CATEGORY(Ts&&... xs)                                    \
      : exception {std::forward<Ts>(xs)...}                                    \
    {}                                                                         \
                                                                               \
    virtual void raise() const override                                        \
    {                                                                          \
      throw *this;                                                             \
    }                                                                          \
  };                                                                           \
                                                                               \
  DEFINE_EXCEPTION_WRITER(CATEGORY, #CATEGORY)

  DEFINE_EXCEPTION_CATEGORY(error)
  DEFINE_EXCEPTION_CATEGORY(warning)

  #define DEFINE_EXCEPTION_SECTION(PREFIX, CATEGORY)                           \
  struct [[maybe_unused]] PREFIX##_##CATEGORY                                  \
    : public CATEGORY                                                          \
  {                                                                            \
    template <typename... Ts>                                                  \
    explicit constexpr PREFIX##_##CATEGORY(Ts&&... xs)                         \
      : CATEGORY {std::forward<Ts>(xs)...}                                     \
    {}                                                                         \
                                                                               \
    virtual void raise() const override                                        \
    {                                                                          \
      throw *this;                                                             \
    }                                                                          \
  };                                                                           \
                                                                               \
  DEFINE_EXCEPTION_WRITER(PREFIX##_##CATEGORY, #PREFIX "-" #CATEGORY)

  DEFINE_EXCEPTION_SECTION(configuration, error)
  DEFINE_EXCEPTION_SECTION(evaluation, error)
  DEFINE_EXCEPTION_SECTION(reader, error)
  DEFINE_EXCEPTION_SECTION(syntax, error)
  DEFINE_EXCEPTION_SECTION(system, error)

  DEFINE_EXCEPTION_SECTION(configuration, warning)
  DEFINE_EXCEPTION_SECTION(evaluation, warning)
  DEFINE_EXCEPTION_SECTION(reader, warning)
  DEFINE_EXCEPTION_SECTION(syntax, warning)
  DEFINE_EXCEPTION_SECTION(system, warning)

  #define DEFINE_EXCEPTION_ABOUT(ABOUT, SECTION, CATEGORY)                     \
  struct [[maybe_unused]] SECTION##_##CATEGORY##_about_##ABOUT                 \
    : public SECTION##_##CATEGORY                                              \
  {                                                                            \
    template <typename... Ts>                                                  \
    explicit constexpr SECTION##_##CATEGORY##_about_##ABOUT(Ts&&... xs)        \
      : SECTION##_##CATEGORY {std::forward<Ts>(xs)...}                         \
    {}                                                                         \
                                                                               \
    virtual void raise() const override                                        \
    {                                                                          \
      throw *this;                                                             \
    }                                                                          \
  };                                                                           \
                                                                               \
  DEFINE_EXCEPTION_WRITER(SECTION##_##CATEGORY##_about_##ABOUT, #SECTION "-" #CATEGORY "-about-" #ABOUT)

  DEFINE_EXCEPTION_ABOUT(pair, reader, error)
  DEFINE_EXCEPTION_ABOUT(parentheses, reader, error)

  DEFINE_EXCEPTION_ABOUT(assignment, syntax, error)

  DEFINE_EXCEPTION_ABOUT(pair, system, error)
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

