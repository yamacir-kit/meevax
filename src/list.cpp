#include <meevax/kernel/list.hpp>

namespace std
{
  auto cbegin(const meevax::kernel::object& x) -> meevax::homoiconic_iterator
  {
    return x;
  }

  auto begin(const meevax::kernel::object& x) -> meevax::homoiconic_iterator
  {
    return x;
  }

  auto cend(const meevax::kernel::object&) -> meevax::homoiconic_iterator
  {
    return meevax::unit;
  }

  auto end(const meevax::kernel::object&) -> meevax::homoiconic_iterator
  {
    return meevax::unit;
  }
} // namespace std

namespace meevax { inline namespace kernel
{
  inline namespace predicate
  {
    auto equal(const object& x, const object& y) -> bool
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
  } // namespace predicate

  inline namespace selector
  {
    let take(const object& exp, std::size_t size)
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
    let append(const object& x, const object& y)
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

    let reverse(const object& x)
    {
      return x ? append(reverse(cdr(x)), list(car(x))) : unit;
    }

    let zip(const object& x, const object& y)
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
}} // namespace meevax::kernel
