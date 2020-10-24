#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <utility>

#include <meevax/console/escape_sequence.hpp>
#include <meevax/utility/hexdump.hpp>

/* ---- Error ------------------------------------------------------------------
 *
 * TODO: Reduce the hierarchy. warning category is unused.
 *
 * - exception
 *    │
 *    ├── error                                                       (category)
 *    │    │
 *    │    ├── configuration_error                                     (section)
 *    │    │
 *    │    ├── evaluation_error                                        (section)
 *    │    │
 *    │    ├── reader_error                                            (section)
 *    │    │    ├── reader_error_about_pair                              (about)
 *    │    │    └── reader_error_about_parentheses                       (about)
 *    │    │
 *    │    ├── syntax_error                                            (section)
 *    │    │    ├── syntax_error_about_assignment                        (about)
 *    │    │    └── syntax_error_about_internal_define                   (about)
 *    │    │
 *    │    └── kernel_error                                            (section)
 *    │
 *    └── warning                                                     (category)
 *
 * - error
 *    |-- syntax_error
 *
 * -------------------------------------------------------------------------- */

namespace meevax { inline namespace kernel
{
  template <typename... Ts>
  auto cat(Ts&&... xs)
  {
    std::stringstream port {};
    (port << ... << xs);
    return port.str();
  }

  struct error
    : public std::runtime_error
  {
    using std::runtime_error::runtime_error;

    template <typename... Ts>
    explicit error(Ts&&... xs)
      : std::runtime_error { cat(std::forward<decltype(xs)>(xs)...) }
    {}

    virtual ~error() = default;

    virtual void raise() const
    {
      throw *this;
    }
  };

  auto operator <<(std::ostream& port, const error& datum) -> decltype(auto)
  {
    return port << magenta << "#("
                << green << "error "
                << cyan << std::quoted(datum.what())
                << magenta << ")"
                << reset;
  }

  #define BOILERPLATE(CATEGORY)                                                \
  template <typename Tag>                                                      \
  struct CATEGORY##_error                                                      \
    : public error                                                             \
  {                                                                            \
    using error::error;                                                        \
                                                                               \
    virtual void raise() const override                                        \
    {                                                                          \
      throw *this;                                                             \
    }                                                                          \
  };                                                                           \
                                                                               \
  template <typename Tag>                                                      \
  auto operator <<(std::ostream& port, const CATEGORY##_error<Tag>& datum)     \
    -> decltype(auto)                                                          \
  {                                                                            \
    return port << magenta << "#("                                             \
                << green << #CATEGORY "-error "                                \
                << cyan << std::quoted(datum.what())                           \
                << magenta << ")"                                              \
                << reset;                                                      \
  } static_assert(true)

  BOILERPLATE(file);
  BOILERPLATE(read);
  BOILERPLATE(syntax);

  #undef BOILERPLATE
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
