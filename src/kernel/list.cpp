#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  auto equal(let const& x, let const& y) -> bool
  {
    if (x.is<null>() and y.is<null>())
    {
      return true;
    }
    else if (x.is<pair>() and y.is<pair>())
    {
      return equal(car(x), car(y)) and equal(cdr(x), cdr(y));
    }
    else
    {
      return eqv(x, y);
    }
  }

  inline namespace selector
  {
    let take(let const& exp, std::size_t size)
    {
      if (0 < size)
      {
        return car(exp) | take(cdr(exp), --size);
      }
      else
      {
        return unit;
      }
    }
  } // namespace selector

  inline namespace miscellaneous
  {
    let append(let const& x, let const& y)
    {
      if (x.is<null>())
      {
        return y;
      }
      else
      {
        return cons(car(x), append(cdr(x), y));
      }
    }

    let reverse(let const& x)
    {
      return x ? append(reverse(cdr(x)), list(car(x))) : unit;
    }

    let zip(let const& x, let const& y)
    {
      if (x.is<null>() and y.is<null>())
      {
        return unit;
      }
      else if (x.is<pair>() and y.is<pair>())
      {
        return list(car(x), car(y)) | zip(cdr(x), cdr(y));
      }
      else
      {
        return unit;
      }
    }
  } // namespace miscellaneous
} // namespace kernel
} // namespace meevax
