#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <sstream>

#include <meevax/system/object.hpp>

#include <meevax/utility/requires.hpp>

// - exception
//    |-- error (category)
//    |    |-- config_error (section)
//    |    |-- reader_error (section)
//    |    |-- syntax_error (section)
//    |    `-- system_error (section)
//    `-- warning (category)

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
              << highlight::simple_datum << std::quoted(exception.what())      \
              << highlight::syntax << ")"                                      \
              << attribute::normal;                                            \
  }

  DEFINE_EXCEPTION_WRITER(exception, "exception")

  #define DEFINE_EXCEPTION_CATEGORY(CATEGORY)                                  \
  struct CATEGORY                                                              \
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
  struct PREFIX##_##CATEGORY                                                   \
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

  DEFINE_EXCEPTION_SECTION(config, error)
  DEFINE_EXCEPTION_SECTION(reader, error)
  DEFINE_EXCEPTION_SECTION(syntax, error)
  DEFINE_EXCEPTION_SECTION(system, error)

  enum class [[deprecated]] category
  {
    pair, parentheses,
  };

  template <category>
  DERIVE([[deprecated]] read_error, public, error)

  // DERIVE([[deprecated]] syntax_error, public, error)
  //
  // std::ostream& operator<<(std::ostream& os, const exception& exception)
  // {
  //   return os << highlight::syntax << "#("
  //             << highlight::constructor << "exception"
  //             << highlight::simple_datum << " \"" << exception.what() << "\""
  //             << highlight::syntax << ")"
  //             << attribute::normal;
  // }
  //
  // std::ostream& operator<<(std::ostream& os, const error& error)
  // {
  //   return os << highlight::syntax << "#("
  //             << highlight::constructor << "error"
  //             << highlight::simple_datum << " \"" << error.what() << "\""
  //             << highlight::syntax << ")"
  //             << attribute::normal;
  // }

  template <category Category>
  std::ostream& operator<<(std::ostream& os, const read_error<Category>& error)
  {
    os << highlight::syntax << "#("
       << highlight::constructor << "read-error"
       << highlight::comment << " #;(category ";

    switch (Category)
    {
    case category::pair:
      os << "pair";
      break;

    case category::parentheses:
      os << "parentheses";
      break;

    default:
      os << "unknown";
      break;
    }

    return os << ") "
              << highlight::simple_datum << "\"" << error.what() << "\""
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

