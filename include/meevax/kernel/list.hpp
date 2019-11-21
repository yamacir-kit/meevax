#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <iterator> // std::begin, std::end, std::distance

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/pair.hpp>

#include <meevax/lambda/compose.hpp>

// Constructors
//   - cons
//   - list
//   - xcons
//   - cons* ... variadic version of cons
//
// Predicators
//   - pair? ... object::is<pair>
//   - null? ... object::operator bool
//   - euqal? ... is_same
//
// Selectors
//   - car ... in meevax/kernel/pair.hpp
//   - cdr ... in meevax/kernel/pair.hpp
//   - cxr ... by preprocessor macro
//   - take
//
// Miscellaneous
//   - length
//   - append
//   - reverse
//   - zip
//
// Fold, unfold, and map
//
// Filtering & partitioning
//
// Searching
//
// Deletion
//
// Association lists
//   - assq
//
// Set operations on lists
//   unimplemented
//
// Primitive side-effects
//   - set-car! ... object::operator=
//   - set-cdr! ... object::operator=
//

namespace meevax::kernel
{
  auto caar = lambda::compose(car, car);
  auto cadr = lambda::compose(car, cdr);
  auto cdar = lambda::compose(cdr, car);
  auto cddr = lambda::compose(cdr, cdr);

  auto caaar = lambda::compose(car, caar);
  auto caadr = lambda::compose(car, cadr);
  auto cadar = lambda::compose(car, cdar);
  auto caddr = lambda::compose(car, cddr);
  auto cdaar = lambda::compose(cdr, caar);
  auto cdadr = lambda::compose(cdr, cadr);
  auto cddar = lambda::compose(cdr, cdar);
  auto cdddr = lambda::compose(cdr, cddr);

  auto caaaar = lambda::compose(car, caaar);
  auto caaadr = lambda::compose(car, caadr);
  auto caadar = lambda::compose(car, cadar);
  auto caaddr = lambda::compose(car, caddr);
  auto cadaar = lambda::compose(car, cdaar);
  auto cadadr = lambda::compose(car, cdadr);
  auto caddar = lambda::compose(car, cddar);
  auto cadddr = lambda::compose(car, cdddr);
  auto cdaaar = lambda::compose(cdr, caaar);
  auto cdaadr = lambda::compose(cdr, caadr);
  auto cdadar = lambda::compose(cdr, cadar);
  auto cdaddr = lambda::compose(cdr, caddr);
  auto cddaar = lambda::compose(cdr, cdaar);
  auto cddadr = lambda::compose(cdr, cdadr);
  auto cdddar = lambda::compose(cdr, cddar);
  auto cddddr = lambda::compose(cdr, cdddr);

  // TODO Rename to homoiconic_iterator, and using list = homoiconic_iterator
  struct iterator
    : public object
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = object;

    using reference = value_type&;
    using const_reference = const reference;

    using pointer = value_type; // represents homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    template <typename... Ts>
    constexpr iterator(Ts&&... operands)
      : object {std::forward<decltype(operands)>(operands)...}
    {}

    decltype(auto) operator*() const
    {
      return car(*this);
    }

    decltype(auto) operator->() const
    {
      return operator*();
    }

    decltype(auto) operator++()
    {
      return *this = cdr(*this);
    }

    decltype(auto) begin() const noexcept
    {
      return *this;
    }

    const iterator end() const noexcept
    {
      return unit;
    }
  };

  iterator begin(const object& object) noexcept
  {
    return object;
  }

  iterator end(const object&) noexcept
  {
    return unit;
  }

  object operator bitor(const object& lhs, const object& rhs)
  {
    return std::make_shared<pair>(lhs, rhs);
  }

  template <typename... Ts>
  constexpr decltype(auto) cons(Ts&&... operands) // is also cons*
  {
    return (operands | ...);
  }

  // TODO Rename to "list_of"
  template <typename... Ts>
  constexpr decltype(auto) list(Ts&&... operands)
  {
    return (operands | ... | unit);
  }

  auto make_list = [](std::size_t size, const object& fill)
  {
    object result;

    for (std::size_t k {0}; k < size; ++k)
    {
      result = cons(fill, result);
    }

    return result;
  };

  template <typename... Ts>
  constexpr decltype(auto) xcons(Ts&&... operands)
  {
    return (... | operands);
  }

  bool is_same(const object& x, const object& y) // equal?
  {
    if (not x and not y)
    {
      return true;
    }
    else if (x.is<pair>() and y.is<pair>())
    {
      return is_same(car(x), car(y)) and is_same(cdr(x), cdr(y));
    }
    else
    {
      return x.equals(y); // eqv?
    }
  }

  object take(const object& exp, std::size_t size)
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

  decltype(auto) length(const iterator& e)
  {
    return std::distance(std::begin(e), std::end(e));
  }

  template <typename List1, typename List2>
  object append(List1&& list1, List2&& list2 = unit)
  {
    if (not list1)
    {
      return list2;
    }
    else
    {
      return car(list1) | append(cdr(list1), list2);
    }
  }

  template <typename List>
  decltype(auto) reverse(List&& list)
  {
    if (not list)
    {
      return list;
    }
    else
    {
      auto buffer {car(list)};

      for (auto& head {cdr(list)}; head; head = cdr(head))
      {
        buffer = cons(head, buffer);
      }

      return buffer;
    }
  }

  object zip(const object& x, const object& y)
  {
    if (!x && !y)
    {
      return unit;
    }
    else if (x.is<pair>() && y.is<pair>())
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return unit;
    }
  }

  template <typename Procedure, typename List>
  object map(Procedure procedure, List&& list)
  {
    if (not list)
    {
      return unit;
    }
    else
    {
      return procedure(car(list)) | map(procedure, cdr(list));
    }
  }

  // template <typename Procedure, typename List1, typename List2, typename... Lists>
  // object map(Procedure procedure, List1&& list1, List2&& list2, Lists&&... lists)
  // {
  //   // TODO
  // }

  const object& assoc(const object& var, const object& env)
  {
    if (!var)
    {
      return unit;
    }
    if (!env)
    {
      return unbound;
    }
    else if (caar(env) == var)
    {
      return cadar(env);
    }
    else
    {
      return assoc(var, cdr(env));
    }
  }

  const object& assq(const object& key,
                     const object& alist)
  {
    if (!key or !alist)
    {
      return false_object;
    }
    else if (caar(alist) == key)
    {
      return car(alist);
    }
    else
    {
      return assq(key, cdr(alist));
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP

