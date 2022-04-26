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

#ifndef INCLUDED_MEEVAX_VISUAL_GEOMETRY_HPP
#define INCLUDED_MEEVAX_VISUAL_GEOMETRY_HPP

#include <utility>

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/register/point.hpp>

#include <Eigen/Core>

BOOST_GEOMETRY_REGISTER_POINT_2D(Eigen::Vector2d, double, cs::cartesian, x(), y())

namespace meevax::visual
{
  using vector = Eigen::Vector2d;
  using point = vector;

  using polygon = boost::geometry::model::polygon<point>;

  class geometry
  {
    using pointer = point*;

    pointer const position_;

  public:
    polygon extents;

    geometry(pointer p)
      : position_ {p}
    {}

    auto& position()
    {
      return *position_;
    }
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_GEOMETRY_HPP

