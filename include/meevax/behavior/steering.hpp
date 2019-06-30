#ifndef INCLUDED_MEEVAX_BEHAVIOR_STEERING_HPP
#define INCLUDED_MEEVAX_BEHAVIOR_STEERING_HPP

#include <Eigen/Core>

namespace meevax::behavior
{
  auto seek(const Eigen::Vector2d& target, double speed_max = 4)
    -> Eigen::Vector2d
  {
    return target.normalized() * speed_max;
  }
} // namespace meevax::behavior

#endif // INCLUDED_MEEVAX_BEHAVIOR_STEERING_HPP

