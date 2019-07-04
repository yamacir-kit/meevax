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

    // steering of pair
    {
      auto steering {visual::arrive(pair.position, surface.center, 100)};
      pair.position += steering * 4;
    }

    context.set_source_rgb(0xe5 / 256.0, 0x50 / 256.0, 0x39 / 256.0);
    context.arc(pair.position[0], pair.position[1], 10, 0, boost::math::constants::two_pi<double>());
    context.fill();


    auto head {visualize(surface, std::get<0>(pair))};
    auto tail {visualize(surface, std::get<1>(pair))};


    auto head_a {visual::flee(head.position(), pair.position)};
    // auto head_a_current_distance {(head.position() - pair.position).norm()};
    // auto head_a_desired_distance {100.0}; // [px]
    // auto head_a_force {head_a_desired_distance / head_a_current_distance};

    auto head_b {visual::flee(head.position(), tail.position())};
    // auto head_b_current_distance {(head.position() - tail.position()).norm()};
    // auto head_b_desired_distance {100.0}; // [px]
    // auto head_b_force {head_b_desired_distance / head_b_current_distance};

    auto head_c {visual::arrive(head.position(), surface.center, 100)};

    visual::vector head_steering {
        head_a + head_b + head_c * 4
    };

    // auto head_steering {
    //                                                            head_a_force * head_a
    //   + std::max<double>(1 - head_a_force,                0) * head_b_force * head_b
    //   + std::max<double>(1 - head_a_force - head_b_force, 0)                * head_c
    // };


    auto tail_a {visual::flee(tail.position(), pair.position)};
    // auto tail_a_current_distance {(tail.position() - pair.position).norm()};
    // auto tail_a_desired_distance {100.0}; // [px]
    // auto tail_a_force {tail_a_desired_distance / tail_a_current_distance};

    auto tail_b {visual::flee(tail.position(), head.position())};
    // auto tail_b_current_distance {(tail.position() - head.position()).norm()};
    // auto tail_b_desired_distance {100.0}; // [px]
    // auto tail_b_force {tail_b_desired_distance / tail_b_current_distance};

    auto tail_c {visual::arrive(tail.position(), surface.center, 100)};

    visual::vector tail_steering {
        tail_a + tail_b + tail_c * 4
    };
    // auto tail_steering {
    //                                                            tail_a_force * tail_a
    //   + std::max<double>(1 - tail_a_force,                0) * tail_b_force * tail_b
    //   + std::max<double>(1 - tail_a_force - tail_b_force, 0)                * tail_c
    // };


    head.position() += head_steering;
    tail.position() += tail_steering;

    context.move_to(pair.position[0], pair.position[1]);
    context.line_to(head.position()[0], head.position()[1]);
    context.stroke();

    context.move_to(pair.position[0], pair.position[1]);
    context.line_to(tail.position()[0], tail.position()[1]);
    context.stroke();

    return {&pair.position};
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

