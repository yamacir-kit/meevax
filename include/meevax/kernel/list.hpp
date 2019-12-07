#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <functional>
#include <iterator> // std::begin, std::end, std::distance

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/pair.hpp>

#include <meevax/lambda/compose.hpp>

/* ==== SRFI-1 ================================================================
*
*   - euqal?                           => recursively_equivalent(const object&, const object&)
*
* Constructors
*   - circular-list
*   - cons                             => cons(Ts&&...)
*   - cons*                            => cons(Ts&&...)
*   - iota
*   - list                             => list(Ts&&...)
*   - list-copy
*   - list-tabulate
*   - make-list
*   - xcons                            => xcons(Ts&&...)
*
* Predicates
*   - circular-list?
*   - dotted-list?
*   - list=
*   - not-pair?
*   - null-list?
*   - null?                            => object::operator bool()
*   - pair?                            => object::is<pair>()
*   - proper-list?
*
* Selectors
*   - car                              => car(const object&)
*   - car+cdr
*   - cdr                              => cdr(const object&)
*   - cxr
*   - drop
*   - drop-right
*   - drop-right!
*   - first ~ tenth
*   - last
*   - last-pair
*   - list-ref
*   - split-at
*   - split-at!
*   - take
*   - take
*   - take!
*   - take-right
*
* Miscellaneous
*   - append
*   - append!
*   - append-reverse
*   - append-reverse!
*   - concatenate
*   - concatenate!
*   - count
*   - length
*   - length+
*   - reverse
*   - reverse!
*   - unzip1
*   - unzip2
*   - unzip3
*   - unzip4
*   - unzip5
*   - zip
*
* Fold, unfold, and map
*   - append-map
*   - append-map!
*   - filter-map
*   - fold
*   - fold-right
*   - for-each
*   - map
*   - map!
*   - map-in-order
*   - pair-fold
*   - pair-fold-right
*   - pair-for-each
*   - reduce
*   - reduce-right
*   - unfold
*   - unfold-right
*
* Filtering & partitioning
*   - filter
*   - filter!
*   - partition
*   - partition!
*   - remove
*   - remove!
*
* Searching
*   - any
*   - break
*   - break!
*   - drop-while
*   - every
*   - find
*   - find-tail
*   - list-index
*   - member
*   - memq
*   - memv
*   - span
*   - span!
*   - take-while
*   - take-while!
*
* Deletion
*   - delete
*   - delete!
*   - delete-duplicates
*   - delete-duplicates!
*
* Association lists
*   - alist-cons
*   - alist-copy
*   - alist-delete
*   - alist-delete!
*   - assoc
*   - assq
*   - assv
*
* Set operations on lists
*   - lset-adjoin
*   - lset-diff+intersection
*   - lset-diff+intersection!
*   - lset-difference
*   - lset-difference!
*   - lset-intersection
*   - lset-intersection!
*   - lset-union
*   - lset-union!
*   - lset-xor
*   - lset-xor!
*   - lset<=
*   - lset=
*
* Primitive side-effects
*   - set-car!                         ... TODO
*   - set-cdr!                         ... TODO
*
*============================================================================ */

namespace meevax::kernel
{
  /* ==== Cxr Library Procedures ==============================================
  *
  * Arbitrary compositions up to four deep are provided.
  *
  *========================================================================== */
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

  /* ==== The Homoiconic Iterator =============================================
  *
  * TODO std::empty
  *
  *========================================================================== */
  struct homoiconic_iterator
    : public object
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = object;

    using reference
      = std::add_lvalue_reference<value_type>::type;

    using const_reference
      = std::add_const<reference>::type;

    using pointer = value_type; // homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    template <typename... Ts>
    constexpr homoiconic_iterator(Ts&&... operands)
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

    const homoiconic_iterator end() const noexcept
    {
      return unit;
    }
  };

  homoiconic_iterator begin(const object& object) noexcept
  {
    return object;
  }

  homoiconic_iterator end(const object&) noexcept
  {
    return unit;
  }

  object operator |(const object& lhs, const object& rhs)
  {
    return std::make_shared<pair>(lhs, rhs);
  }

  template <typename... Ts>
  constexpr decltype(auto) cons(Ts&&... operands) // is also cons*
  {
    return (operands | ...);
  }

  /* ==== The List Type =======================================================
  *
  * TODO Documentations
  *
  *========================================================================== */
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

  template <typename T, typename U>
  decltype(auto) equivalent(T&& x, U&& y)
  {
    return x.equivalent_to(y);
  }

  bool recursively_equivalent(
    const object& x,
    const object& y)
  {
    if (not x and not y)
    {
      return true;
    }
    else if (x.is<pair>() and y.is<pair>())
    {
      return
            recursively_equivalent(car(x), car(y))
        and recursively_equivalent(cdr(x), cdr(y));
    }
    else
    {
      return x.equivalent_to(y);
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

  decltype(auto) length(const homoiconic_iterator& e)
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

  const object&
    assoc(
      const object& value,
      const object& association_list)
  {
    if (not value)
    {
      return unit;
    }
    if (not association_list)
    {
      return unbound;
    }
    else if (recursively_equivalent(
               caar(association_list),
               value))
    {
      return cadar(association_list);
    }
    else
    {
      return
        assoc(
          value,
          cdr(association_list));
    }
  }

  const object&
    assq(
      const object& value,
      const object& association_list)
  {
    if (not value or not association_list)
    {
      return false_object;
    }
    else if (caar(association_list) == value)
    {
      return car(association_list);
    }
    else
    {
      return
        assq(
          value,
          cdr(association_list));
    }
  }

  template <auto Coarseness = 0>
  struct equivalence_comparator;

  #define SPECIALIZE_EQUIVALENCE_COMPARATOR(COARSENESS, COMPARE)               \
  template <>                                                                  \
  struct equivalence_comparator<COARSENESS>                                    \
  {                                                                            \
    template <typename... Ts>                                                  \
    decltype(auto) operator ()(Ts&&... operands)                               \
    {                                                                          \
      return                                                                   \
        std::invoke(                                                           \
          COMPARE,                                                             \
          std::forward<decltype(operands)>(operands)...);                      \
    }                                                                          \
  }

  SPECIALIZE_EQUIVALENCE_COMPARATOR(0, std::equal_to {});
  SPECIALIZE_EQUIVALENCE_COMPARATOR(1,             equivalent);
  SPECIALIZE_EQUIVALENCE_COMPARATOR(2, recursively_equivalent);

  #undef SPECIALIZE_EQUIVALENCE_COMPARATOR

  using default_equivalence_comparator = equivalence_comparator<>;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP

