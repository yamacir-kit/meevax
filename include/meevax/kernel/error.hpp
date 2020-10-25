#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <iomanip>
#include <stdexcept>
#include <utility>

#include <meevax/console/escape_sequence.hpp>
#include <meevax/kernel/preface.hpp>
#include <meevax/string/cat.hpp>
#include <meevax/utility/hexdump.hpp>

/* ---- Error ------------------------------------------------------------------
 *
 * - error
 *    |-- file-error
 *    |-- read_error
 *    `-- syntax_error
 *
 * -------------------------------------------------------------------------- */

namespace meevax { inline namespace kernel
{
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
