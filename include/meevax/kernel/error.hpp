#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>
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
  struct error_
    : public virtual pair
  {
    // explicit error_() = default;
    //
    // template <typename S, REQUIRES(std::is_convertible<S, std::string>)>
    // explicit error_(S const& message, let const& irritants = unit)
    //   : pair { make<string>(message), irritants }
    // {}

    // template <typename... Ts>
    // explicit error_(std::string const& message, Ts&&... xs)
    //   : pair { make<string>(message), list(std::forward<decltype(xs)>(xs)...) }
    // {}

    // explicit error_(let const& kar, let const& kdr = unit)
    //   : pair { kar, kdr }
    // {}

    template <typename... Ts>
    explicit error_(Ts&&... xs)
      : pair { std::forward<decltype(xs)>(xs)... }
    {}

    // using pair::pair;

    ~error_() override = default;

    virtual void raise() const
    {
      throw *this;
    }

    friend auto operator <<(output_port & port, error_ const& datum) -> output_port &
    {
      port << magenta << "#,(" << green << "error " << reset << car(datum);

      for (let const& each : cdr(datum))
      {
        port << " " << each;
      }

      return port << magenta << ")" << reset;
    }
  };

  struct read_error_ : public error_
  {
    template <typename... Ts>
    explicit read_error_(Ts&&... xs)
      : error_ { std::forward<decltype(xs)>(xs)... }
    {}

    ~read_error_() override = default;
  };

  template <typename... Ts>
  struct tagged_read_error_ : public read_error_
  {
    template <typename... Us>
    explicit tagged_read_error_(Us&&... xs)
      : read_error_ { std::forward<decltype(xs)>(xs)... }
    {}

    ~tagged_read_error_() override = default;
  };

  struct file_error_ : public error_
  {
    template <typename... Us>
    explicit file_error_(Us&&... xs)
      : error_ { std::forward<decltype(xs)>(xs)... }
    {}

    ~file_error_() override = default;
  };

  struct syntax_error_ : public error_
  {
    using error_::error_;

    ~syntax_error_() override = default;
  };

  template <typename... Ts>
  struct tagged_syntax_error_ : public syntax_error_
  {
    using syntax_error_::syntax_error_;

    ~tagged_syntax_error_() override = default;
  };

  struct [[deprecated]] error : public std::runtime_error
  {
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

  auto operator <<(output_port &, error const&) -> output_port &;

  #define BOILERPLATE(CATEGORY)                                                \
  template <typename Tag>                                                      \
  struct [[deprecated]] CATEGORY##_error : public error                        \
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
  auto operator <<(output_port & port, CATEGORY##_error<Tag> const& datum)     \
    -> output_port &                                                           \
  {                                                                            \
    return port << magenta << "#,("                                            \
                << green << #CATEGORY "-error "                                \
                << cyan << std::quoted(datum.what())                           \
                << magenta << ")"                                              \
                << reset;                                                      \
  } static_assert(true)

  BOILERPLATE(file);
  BOILERPLATE(read);
  BOILERPLATE(syntax);

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
