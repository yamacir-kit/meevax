#include <functional>
#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/real.hpp>
#include <meevax/system/srfi-1.hpp>

extern "C"
{
  PROCEDURE(addition)
  {
    using namespace meevax::system;
    return std::accumulate(std::begin(args), std::end(args), make<real>(0), std::plus {});
  }

  PROCEDURE(multiplication)
  {
    using namespace meevax::system;
    return std::accumulate(std::begin(args), std::end(args), make<real>(1), std::multiplies {});
  }

  PROCEDURE(subtraction)
  {
    using namespace meevax::system;

    if (length(args) < 2)
    {
      return std::accumulate(std::begin(args), std::end(args), make<real>(0), std::minus {});
    }
    else
    {
      return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::minus {});
    }
  }

  PROCEDURE(division)
  {
    using namespace meevax::system;

    if (length(args) < 2)
    {
      return std::accumulate(std::begin(args), std::end(args), make<real>(1), std::divides {});
    }
    else
    {
      return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::divides {});
    }
  }

  PROCEDURE(less)
  {
    using namespace meevax::system;
    return std::invoke(std::less {}, car(args).as<real>(), cadr(args).as<real>()) ? _true_ : _false_;
  }

  PROCEDURE(less_equal)
  {
    using namespace meevax::system;
    return std::invoke(std::less_equal {}, car(args).as<real>(), cadr(args).as<real>()) ? _true_ : _false_;
  }

  PROCEDURE(greater)
  {
    using namespace meevax::system;
    return std::invoke(std::greater {}, car(args).as<real>(), cadr(args).as<real>()) ? _true_ : _false_;
  }

  PROCEDURE(greater_equal)
  {
    using namespace meevax::system;
    return std::invoke(std::greater_equal {}, car(args).as<real>(), cadr(args).as<real>()) ? _true_ : _false_;
  }

  PROCEDURE(real_)
  {
    using namespace meevax::system;
    return car(args).is<real>() ? _true_ : _false_;
  }
} // extern "C"

