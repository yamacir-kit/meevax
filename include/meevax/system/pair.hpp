#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <iostream>
#include <utility>

#include <boost/math/constants/constants.hpp>

#include <meevax/system/pointer.hpp>
#include <meevax/system/exception.hpp>

#include <meevax/visual/behavior.hpp>
#include <meevax/visual/context.hpp>
#include <meevax/visual/geometry.hpp>
#include <meevax/visual/surface.hpp>

namespace meevax::system
{
  struct pair;

  /**
   * The pair type is always underlies any object type (is performance hack).
   *
   * We implemented heterogenous pointer by type-erasure, this is very flexible
   * but, requires dynamic-cast to restore erased type in any case. So, we
   * decided to remove typecheck for pair type, by always waste memory space
   * for two heterogenous pointer slot (yes, is cons-cell). If pair selector
   * (car/cdr) always requires typecheck, our system will be unbearlably slowly.
   * Built-in types are designed to make the best possible use of the fact that
   * these are pair as well (e.g. closure is pair of expression and lexical
   * environment, string is linear-list of character, complex, rational).
   */
  using object = pointer<pair>;

  extern "C" const object unit, unbound, undefined, unspecified;

  struct pair
    : public std::pair<object, object>
    , public facade<pair>
  {
    template <typename... Ts>
    explicit constexpr pair(Ts&&... args)
      : std::pair<object, object> {std::forward<Ts>(args)...}
    {}

    pair()
      : std::pair<object, object> {unit, unit}
    {}

    visual::point position;
  };

  template <typename T, typename... Ts>
  constexpr decltype(auto) make(Ts&&... args)
  {
    return object::bind<T>(std::forward<Ts>(args)...);
  }

  #ifndef NDEBUG
  #define SELECTOR(NAME, INDEX)                                                \
  template <typename Pointer>                                                  \
  decltype(auto) NAME(Pointer&& object)                                        \
  {                                                                            \
    if (object)                                                                \
    {                                                                          \
      return std::get<INDEX>(object.dereference());                            \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw error {"internal illegal selection rejected"};                     \
    }                                                                          \
  }
  #else
  #define SELECTOR(NAME, INDEX)                                                \
  template <typename Pointer>                                                  \
  decltype(auto) NAME(Pointer&& object)                                        \
  {                                                                            \
    return std::get<INDEX>(object.dereference());                              \
  }
  #endif // NDEBUG

  SELECTOR(car, 0)
  SELECTOR(cdr, 1)

  auto operator<<(std::ostream& os, const pair& pare)
    -> decltype(os)
  {
    os << "\x1b[35m(\x1b[0m" << std::get<0>(pare);

    for (auto object {std::get<1>(pare)}; object; object = cdr(object))
    {
      if (object.is<pair>())
      {
        os << " " << car(object);
      }
      else // iter is the last element of dotted-list.
      {
        os << "\x1b[35m . \x1b[0m" << object;
      }
    }

    return os << "\x1b[35m)\x1b[0m";
  }

  auto visualize(visual::surface& surface, pair& pair)
    -> visual::geometry
  {
    visual::context context {surface};

    static constexpr double length {50};

    context.set_source_rgb(0.5, 0.5, 0.5);

    context.set_line_width(3);
    context.rectangle(pair.position[0] - length, pair.position[1] - length * 0.5, length, length);
    context.rectangle(pair.position[0],          pair.position[1] - length * 0.5, length, length);

    context.new_sub_path();
    context.arc(pair.position[0] - length * 0.5, pair.position[1], 5, 0, boost::math::constants::two_pi<double>());

    context.new_sub_path();
    context.arc(pair.position[0] + length * 0.5, pair.position[1], 5, 0, boost::math::constants::two_pi<double>());


    auto head {visualize(surface, std::get<0>(pair))};
    auto tail {visualize(surface, std::get<1>(pair))};

    visual::vector head_steering {
        visual::flee(head.position(), pair.position)
      + visual::flee(head.position(), tail.position())
      + visual::arrive(head.position(), surface.center, 100) * 4
    };

    visual::vector tail_steering {
        visual::flee(tail.position(), pair.position)
      + visual::flee(tail.position(), head.position())
      + visual::arrive(tail.position(), surface.center, 100) * 4
    };

    visual::vector pair_steering {
        visual::flee(pair.position, head.position())
      + visual::flee(pair.position, tail.position())
      + visual::arrive(pair.position, surface.center, 100) * 4
    };

    pair.position += pair_steering;
    head.position() += head_steering;
    tail.position() += tail_steering;

    context.set_source_rgb(0.25, 0.25, 0.25);

    context.move_to(pair.position[0] - length * 0.5, pair.position[1]);
    context.line_to(head.position()[0], head.position()[1]);
    context.stroke();

    context.move_to(pair.position[0] + length * 0.5, pair.position[1]);
    context.line_to(tail.position()[0], tail.position()[1]);
    context.stroke();

    return {&pair.position};
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

