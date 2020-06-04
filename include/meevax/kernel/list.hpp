#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <algorithm> // std::equal
#include <iterator> // std::begin, std::end, std::distance

#include <meevax/functional/compose.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/pair.hpp>

/* ==== SRFI-1 ================================================================
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
* Fold, unfold & map
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
  /* ==== Cxr Library Procedures ===============================================
  *
  * Arbitrary compositions up to four deep are provided.
  *
  *========================================================================== */
  auto caar = functional::compose(car, car);
  auto cadr = functional::compose(car, cdr);
  auto cdar = functional::compose(cdr, car);
  auto cddr = functional::compose(cdr, cdr);

  auto caaar = functional::compose(car, caar);
  auto caadr = functional::compose(car, cadr);
  auto cadar = functional::compose(car, cdar);
  auto caddr = functional::compose(car, cddr);
  auto cdaar = functional::compose(cdr, caar);
  auto cdadr = functional::compose(cdr, cadr);
  auto cddar = functional::compose(cdr, cdar);
  auto cdddr = functional::compose(cdr, cddr);

  auto caaaar = functional::compose(car, caaar);
  auto caaadr = functional::compose(car, caadr);
  auto caadar = functional::compose(car, cadar);
  auto caaddr = functional::compose(car, caddr);
  auto cadaar = functional::compose(car, cdaar);
  auto cadadr = functional::compose(car, cdadr);
  auto caddar = functional::compose(car, cddar);
  auto cadddr = functional::compose(car, cdddr);
  auto cdaaar = functional::compose(cdr, caaar);
  auto cdaadr = functional::compose(cdr, caadr);
  auto cdadar = functional::compose(cdr, cadar);
  auto cdaddr = functional::compose(cdr, caddr);
  auto cddaar = functional::compose(cdr, cdaar);
  auto cddadr = functional::compose(cdr, cdadr);
  auto cdddar = functional::compose(cdr, cddar);
  auto cddddr = functional::compose(cdr, cdddr);

  /* ==== The Homoiconic Iterator ==============================================
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

  // TODO move into namespace std?
  homoiconic_iterator begin(const object& object) noexcept
  {
    return object;
  }

  // TODO move into namespace std?
  homoiconic_iterator end(const object&) noexcept
  {
    return unit;
  }

  /* ==== Constructors =========================================================
  *
  * From R7RS
  *   - cons                            => cons
  *   - list                            => list
  *
  * From SRFI-1
  *   - circular-list
  *   - cons*                           => cons
  *   - iota
  *   - list-copy
  *   - list-tabulate
  *   - make-list
  *   - xcons                           => xcons
  *
  *========================================================================== */
  inline namespace constructor
  {
    inline decltype(auto) operator |(const object& lhs, const object& rhs)
    {
      return std::make_shared<pair>(lhs, rhs);
    }

    auto cons = [](auto&&... xs) constexpr
    {
      return (xs | ...);
    };

    auto list = [](auto&& ... xs) constexpr
    {
      return (xs | ... | unit);
    };

    auto make_list = [](std::size_t size, const object& x = unit)
    {
      object result;

      for (std::size_t i {0}; i < size; ++i)
      {
        result = cons(x, result);
      }

      return result;
    };

    auto xcons = [](auto&&... xs) constexpr
    {
      return (... | xs);
    };
  }

  /* ==== Predicates ===========================================================
   *
   * From SRFI-1
   *   - circular-list?
   *   - dotted-list?
   *   - eq?                            => object::operator ==
   *   - eqv?                           => object::equivalent_to
   *   - euqal?                         => equal
   *   - list?
   *   - not-pair?
   *   - null-list?
   *   - null?                          => object::operator bool
   *   - pair?                          => object::is<pair>
   *   - proper-list?
   *
   * ======================================================================== */
  inline namespace predicate
  {
    auto equivalent = [](auto&& x, auto&& y)
    {
      return x.equivalent_to(y);
    };

    bool equal(const object& x, const object& y)
    {
      if (not x and not y)
      {
        return true;
      }
      else if (x.is<pair>() and y.is<pair>())
      {
        return equal(car(x), car(y)) and equal(cdr(x), cdr(y));
      }
      else
      {
        return x.equivalent_to(y);
      }
    }

    template <auto Coarseness = 0>
    struct equivalence_comparator;

    #define SPECIALIZE_EQUIVALENCE_COMPARATOR(COARSENESS, COMPARE)             \
    template <>                                                                \
    struct equivalence_comparator<COARSENESS>                                  \
    {                                                                          \
      template <typename... Ts>                                                \
      decltype(auto) operator ()(Ts&&... operands)                             \
      {                                                                        \
        return                                                                 \
          std::invoke(                                                         \
            COMPARE,                                                           \
            std::forward<decltype(operands)>(operands)...);                    \
      }                                                                        \
    }

    SPECIALIZE_EQUIVALENCE_COMPARATOR(0, std::equal_to {});
    SPECIALIZE_EQUIVALENCE_COMPARATOR(1, equivalent);
    SPECIALIZE_EQUIVALENCE_COMPARATOR(2, equal);

    #undef SPECIALIZE_EQUIVALENCE_COMPARATOR

    using default_equivalence_comparator = equivalence_comparator<>;
  }

  /* ==== Selectors ============================================================
  *
  * From R7RS
  *   - car                              => car
  *   - cdr                              => cdr
  *   - cxr
  *   - list-ref                         => list_reference
  *   - list-tail                        => list_tail
  *
  * Selectors
  *   - car+cdr
  *   - drop
  *   - drop-right
  *   - drop-right!
  *   - first ~ tenth
  *   - last
  *   - last-pair
  *   - split-at
  *   - split-at!
  *   - take                            => take
  *   - take
  *   - take!
  *   - take-right
  *
  *========================================================================== */
  inline namespace selector
  {
    auto list_tail
      = [](homoiconic_iterator iter, auto n)
    {
      return std::next(iter, n);
    };

    auto list_reference
      = [](const object& x, auto n)
    {
      return car(list_tail(x, n));
    };

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
  }

  /* ==== Miscellaneous ========================================================
  *
  * TODO Documentations
  *
  *========================================================================== */
  inline namespace miscellaneous
  {
    inline decltype(auto) length(const homoiconic_iterator& e)
    {
      return std::distance(std::begin(e), std::end(e));
    }

    const object append(const object& x, const object& y)
    {
      if (not x)
      {
        return y;
      }
      else
      {
        return
          cons(
            car(x),
            append(cdr(x), y));
      }
    }

    template <typename List>
    auto reverse(const List& x)
      -> List
    {
      return x ? append(reverse(cdr(x)), list(car(x))) : unit;
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
  }

  /* ==== Folding ==============================================================
  *
  * TODO Documentations
  *
  *========================================================================== */
  inline namespace folding
  {
  }

  /* ==== Unfolding ============================================================
  *
  * TODO Documentations
  *
  *========================================================================== */
  inline namespace unfolding
  {
  }

  /* ==== Mapping ==============================================================
  *
  * TODO Documentations
  *
  *========================================================================== */
  inline namespace mapping
  {
    template <typename Procedure>
    object map(Procedure procedure, const object& x)
    {
      if (not x)
      {
        return unit;
      }
      else
      {
        return
          cons(
            procedure(
              car(x)),
            map(
              procedure,
              cdr(x)));
      }
    }
  }

  /* ==== Association List =====================================================
  *
  * From R7RS
  *   - assoc                           => assoc
  *   - assq                            => assq
  *   - assv                            => assv
  *
  * From SRFI-1
  *   - alist-cons                      => alist_cons
  *   - alist-copy
  *   - alist-delete
  *   - alist-delete!
  *
  *========================================================================== */
  inline namespace association_list
  {
    [[deprecated]]
    const object& assoc(const object& value, const object& association_list)
    {
      if (not value or not association_list)
      {
        return value;
      }
      else if (equal(caar(association_list), value))
      {
        return car(association_list);
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
      assv(
        const object& value,
        const object& association_list)
    {
      if (not value or not association_list)
      {
        return value;
      }
      else if (caar(association_list).equivalent_to(value))
      {
        return car(association_list);
      }
      else
      {
        return
          assv(
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
        return value;
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

    const object
      alist_cons(
        const object& key,
        const object& value,
        const object& alist)
    {
      return
        cons(
          cons(key, value),
          alist);
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP

