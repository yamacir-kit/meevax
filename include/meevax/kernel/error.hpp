#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <meevax/kernel/pair.hpp>

/* ---- Error ------------------------------------------------------------------
 *
 * - error
 *    |-- file-error
 *    |-- read_error
 *    `-- syntax_error
 *
 * -------------------------------------------------------------------------- */

namespace meevax
{
inline namespace kernel
{
  struct error : public std::runtime_error
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
  struct CATEGORY##_error : public error                                       \
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
