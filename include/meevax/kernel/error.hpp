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
    // explicit error() = default;
    //
    // template <typename S, REQUIRES(std::is_convertible<S, std::string>)>
    // explicit error(S const& message, let const& irritants = unit)
    //   : pair { make<string>(message), irritants }
    // {}

    // template <typename... Ts>
    // explicit error(std::string const& message, Ts&&... xs)
    //   : pair { make<string>(message), list(std::forward<decltype(xs)>(xs)...) }
    // {}

    // explicit error(let const& kar, let const& kdr = unit)
    //   : pair { kar, kdr }
    // {}

    template <typename... Ts>
    explicit error(Ts&&... xs)
      : pair { std::forward<decltype(xs)>(xs)... }
    {}

    // using pair::pair;

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

  struct read_error : public error
  {
    template <typename... Ts>
    explicit read_error(Ts&&... xs)
      : error { std::forward<decltype(xs)>(xs)... }
    {}

    ~read_error() override = default;
  };

  template <typename... Ts>
  struct tagged_read_error : public read_error
  {
    template <typename... Us>
    explicit tagged_read_error(Us&&... xs)
      : read_error { std::forward<decltype(xs)>(xs)... }
    {}

    ~tagged_read_error() override = default;
  };

  struct file_error : public error
  {
    template <typename... Us>
    explicit file_error(Us&&... xs)
      : error { std::forward<decltype(xs)>(xs)... }
    {}

    ~file_error() override = default;
  };

  struct syntax_error : public error
  {
    using error::error;

    ~syntax_error() override = default;
  };

  template <typename... Ts>
  struct tagged_syntax_error : public syntax_error
  {
    using syntax_error::syntax_error;

    ~tagged_syntax_error() override = default;
  };

  // struct [[deprecated]] error : public std::runtime_error
  // {
  //   template <typename... Ts>
  //   explicit error(Ts&&... xs)
  //     : std::runtime_error { cat(std::forward<decltype(xs)>(xs)...) }
  //   {}
  //
  //   virtual ~error() = default;
  //
  //   virtual void raise() const
  //   {
  //     throw *this;
  //   }
  // };
  //
  // auto operator <<(output_port &, error const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
