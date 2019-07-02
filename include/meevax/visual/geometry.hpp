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
  using point = Eigen::Vector2d;

  using polygon = boost::geometry::model::polygon<point>;

  class geometry
  {
    point* const position_;

  public:
    polygon extents;

    geometry(point* p)
      : position_ {p}
    {}

    auto& position()
    {
      return *position_;
    }
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_GEOMETRY_HPP

