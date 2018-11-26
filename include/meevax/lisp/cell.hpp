#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <memory>
#include <string>
#include <tuple>
#include <utility>

#include <meevax/facade/identity.hpp>
// #include <meevax/tuple/iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  struct cell; // forward decreation for type `cusror`

  template <typename T>
  struct iterator
    : public std::shared_ptr<T>,
      public std::iterator<std::input_iterator_tag, typename std::shared_ptr<T>::element_type>
  {
    template <typename... Ts>
    constexpr iterator(Ts&&... args)
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    template <typename U>
    friend decltype(auto) car(const std::shared_ptr<U>& iter) noexcept
    {
      return std::get<0>(iter.std::shared_ptr<U>::operator*());
    }

    template <typename U>
    friend decltype(auto) cdr(const iterator<U>& iter) noexcept
    {
      return std::get<1>(iter.std::shared_ptr<U>::operator*());
    }

    decltype(auto) operator*() const noexcept
    {
      return car(*this);
    }

    decltype(auto) operator++() noexcept
    {
      return *this = cdr(*this);
    }
  };

  using cursor = iterator<cell>;

  // using cursor = tuple::iterator<std::shared_ptr<cell>>;
  const cursor nil {nullptr};

  struct cell
    : public std::pair<cursor, cursor>,
      public facade::identity<cell>
  {
    template <typename... Ts>
    constexpr cell(Ts&&... args)
      : std::pair<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    virtual ~cell() = default; // removable
  };

  const cursor t {std::make_shared<
    utility::binder<std::string, cell>
  >("true")};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

