#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>
#include <stdexcept>
#include <type_traits>

/* ---- Error ------------------------------------------------------------------
 *
 * - error
 *    |-- file-error
 *    |-- read_error
 *    |    `-- tagged_read_error
 *    `-- syntax_error
 *
 * -------------------------------------------------------------------------- */

namespace meevax
{
inline namespace kernel
{
  struct error
    : public virtual pair
  {
    template <typename... Ts>
    explicit error(Ts&&... xs)
      : pair { std::forward<decltype(xs)>(xs)... }
    {}

    ~error() override = default;

    virtual auto what() const -> std::string
    {
      std::stringstream ss { "error: " };
      car(*this).as<const string>().write_string(ss);
      ss << ".";
      return ss.str();
    }

    virtual void raise() const
    {
      throw *this;
    }
  };

  auto operator <<(output_port & port, error const& datum) -> output_port &;

  #define DEFINE_ERROR(TYPENAME)                                               \
  struct TYPENAME ## _error : public error                                     \
  {                                                                            \
    template <typename... Ts>                                                  \
    explicit TYPENAME ## _error(Ts&&... xs)                                    \
      : error { std::forward<decltype(xs)>(xs)... }                            \
    {}                                                                         \
                                                                               \
    ~TYPENAME ## _error() override = default;                                  \
  }

  DEFINE_ERROR(continuable);
  DEFINE_ERROR(file);
  DEFINE_ERROR(read);
  DEFINE_ERROR(syntax);

  template <typename... Ts>
  struct tagged_read_error : public read_error
  {
    template <typename... Us>
    explicit tagged_read_error(Us&&... xs)
      : read_error { std::forward<decltype(xs)>(xs)... }
    {}

    ~tagged_read_error() override = default;
  };

  template <typename... Ts>
  struct tagged_syntax_error : public syntax_error
  {
    using syntax_error::syntax_error;

    ~tagged_syntax_error() override = default;
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
