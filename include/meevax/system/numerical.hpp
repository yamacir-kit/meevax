#ifndef INCLUDED_MEEVAX_SYSTEM_NUMERICAL_HPP
#define INCLUDED_MEEVAX_SYSTEM_NUMERICAL_HPP

#include <utility>

#include <boost/multiprecision/gmp.hpp>
#include <boost/multiprecision/mpfr.hpp>

#include <meevax/system/pair.hpp>

#include <meevax/visual/behavior.hpp>
#include <meevax/visual/context.hpp>
#include <meevax/visual/geometry.hpp>
#include <meevax/visual/surface.hpp>

namespace meevax::system
{
  using integral
    = boost::multiprecision::number<
        boost::multiprecision::gmp_int,
        boost::multiprecision::et_off
      >;

  std::ostream& operator<<(std::ostream& os, const integral& integral)
  {
    return os << "\x1B[36m" << integral.str() << "\x1B[0m";
  }

  using real_base
    = boost::multiprecision::number<
        boost::multiprecision::mpfr_float_backend<0>,
        boost::multiprecision::et_off
      >;

  struct real
    : public real_base
  {
    visual::point position;

    template <typename... Ts>
    explicit constexpr real(Ts&&... xs)
      : real_base {std::forward<Ts>(xs)...}
    {}

    const auto& boost() const noexcept
    {
      return static_cast<const real_base&>(*this);
    }
  };

  std::ostream& operator<<(std::ostream& os, const real& real)
  {
    return os << "\x1B[36m" << real.str() << "\x1B[0m";
  }

  auto visualize(visual::surface& surface, real& real)
    -> visual::geometry
  {
    visual::context context {surface};

    // const auto steering {visual::seek(real.position, surface.center)};
    // real.position += steering;

    context.set_source_rgb(0x4a / 256.0, 0x69 / 256.0, 0xbd / 256.0);
    context.select_font_face("Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    context.set_font_size(32);

    cairo_text_extents_t extents {};
    context.text_extents(real.str().c_str(), &extents);

    context.move_to(
      real.position[0] - extents.width / 2,
      real.position[1] + extents.height / 2
    );
    context.show_text(real.str().c_str());

    return {&real.position};
  }

  #define DEFINE_NUMERIC_BINARY_OPERATOR(OPERATOR) \
  decltype(auto) operator OPERATOR(const object& lhs, const object& rhs) \
  { \
    return make<real>( \
      lhs.as<const real>().boost() OPERATOR rhs.as<const real>().boost() \
    ); \
  }

  DEFINE_NUMERIC_BINARY_OPERATOR(+)
  DEFINE_NUMERIC_BINARY_OPERATOR(*)
  DEFINE_NUMERIC_BINARY_OPERATOR(-)
  DEFINE_NUMERIC_BINARY_OPERATOR(/)
  DEFINE_NUMERIC_BINARY_OPERATOR(<)
  DEFINE_NUMERIC_BINARY_OPERATOR(<=)
  DEFINE_NUMERIC_BINARY_OPERATOR(>)
  DEFINE_NUMERIC_BINARY_OPERATOR(>=)
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_NUMERICAL_HPP

