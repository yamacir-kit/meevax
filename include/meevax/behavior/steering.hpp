#ifndef INCLUDED_MEEVAX_BEHAVIOR_STEERING_HPP
#define INCLUDED_MEEVAX_BEHAVIOR_STEERING_HPP

#include <limits>
#include <random>

#include <meevax/visual/geometry.hpp>

namespace meevax::behavior
{
  auto random_point(double max = 42)
    -> visual::point
  {
    static std::random_device device {};
    static std::uniform_real_distribution<double> random {1, max};

    return {random(device), random(device)};
  }

  void seek(visual::point& character, const visual::point& to, double speed_max = 1)
  {
    if (auto direction {(to - character).normalized()};
        direction.norm() <= std::numeric_limits<double>::epsilon())
    {
      character += random_point().normalized() * speed_max;
    }
    else
    {
      character += direction * speed_max;
    }
  }

  void flee(visual::point& character, const visual::point& from, double speed_max = 1)
  {
    if (auto direction {(character - from).normalized()};
        direction.norm() <= std::numeric_limits<double>::epsilon())
    {
      character += random_point().normalized() * speed_max;
    }
    else
    {
      character += direction * speed_max;
    }
  }
} // namespace meevax::behavior

#endif // INCLUDED_MEEVAX_BEHAVIOR_STEERING_HPP

