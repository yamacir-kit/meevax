#ifndef INCLUDED_MEEVAX_VISUAL_BEHAVIOR_HPP
#define INCLUDED_MEEVAX_VISUAL_BEHAVIOR_HPP

#include <limits>
#include <random>

#include <meevax/visual/geometry.hpp>

namespace meevax::visual
{
  auto random()
    -> visual::point
  {
    static std::random_device device {};
    static std::uniform_real_distribution<double> random {0, 1};

    // auto x {random(device)};
    //
    // return {x, std::sqrt(1 - std::pow(x, 2))};
    return visual::point {random(device), random(device)}.normalized();
  }

  auto seek(const visual::point& character, const visual::point& to, double speed_max = 1)
    -> visual::point
  {
    if (auto direction {(to - character).normalized()}; direction.norm() < std::numeric_limits<double>::epsilon())
    {
      return random() * speed_max;
    }
    else
    {
      return direction * speed_max;
    }
  }

  auto flee(const visual::point& character, const visual::point& from, double speed_max = 1)
    -> visual::point
  {
    if (auto direction {(character - from).normalized()}; direction.norm() < std::numeric_limits<double>::epsilon())
    {
      return random() * speed_max;
    }
    else
    {
      return direction * speed_max;
    }
  }
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_BEHAVIOR_HPP

