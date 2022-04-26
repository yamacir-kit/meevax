/*
   Copyright 2018-2022 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

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

    auto x {random(device)};
    return {x, std::sqrt(1 - std::pow(x, 2))};
    // return visual::point {random(device), random(device)}.normalized();
  }

  auto seek(const visual::point& character, const visual::point& target, double speed_max = 2)
    -> visual::point
  {
    if (auto direction {(target - character).normalized()}; direction.norm() < std::numeric_limits<double>::epsilon())
    {
      return random() * speed_max;
    }
    else
    {
      return direction * speed_max;
    }
  }

  auto flee(const visual::point& character, const visual::point& target, double speed_max = 2)
    -> visual::point
  {
    if (auto direction {(character - target).normalized()}; direction.norm() < std::numeric_limits<double>::epsilon())
    {
      return random() * speed_max;
    }
    else
    {
      return direction * speed_max;
    }
  }

  auto arrive(const visual::point& character, const visual::point& target, double slowing_distance, double speed_max = 2)
  {
    const auto direction {target - character};
    const auto distance {direction.norm()};
    const auto ramped_speed {speed_max * (distance / slowing_distance)};
    const auto clipped_speed {std::min(ramped_speed, speed_max)};
    const auto desired_velocity {(clipped_speed / distance) * direction};

    return desired_velocity;
  }
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_BEHAVIOR_HPP

