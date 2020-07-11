#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <algorithm> // std::equal
#include <iterator> // std::begin, std::end, std::distance

#include <meevax/functional/compose.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  /* ==== The Homoiconic Iterator ==============================================
   *
   * TODO std::empty
   *
   * ======================================================================== */
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
    constexpr homoiconic_iterator(Ts&&... xs)
      : object { std::forward<decltype(xs)>(xs)... }
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

    value_type operator++(int)
    {
      auto result { *this };
      operator ++();
      return result;
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
}} // namespace meevax::kernel

namespace std
{
  auto cbegin(const meevax::kernel::object& x)
    -> meevax::kernel::homoiconic_iterator
  {
    return x;
  }

  auto begin(const meevax::kernel::object& x)
    -> meevax::kernel::homoiconic_iterator
  {
    return x;
  }

  auto cend(const meevax::kernel::object&)
    -> meevax::kernel::homoiconic_iterator
  {
    return meevax::kernel::unit;
  }

  auto end(const meevax::kernel::object&)
    -> meevax::kernel::homoiconic_iterator
  {
    return meevax::kernel::unit;
  }
} // namespace std

namespace meevax { inline namespace kernel
{
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
   * ======================================================================== */
  inline namespace constructor
  {
    inline decltype(auto) operator |(const object& lhs, const object& rhs)
    {
      return std::make_shared<pair>(lhs, rhs);
    }

    auto cons = [](auto&&... xs)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return (xs | ...);
    };

    auto list = [](auto&& ... xs)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return (xs | ... | unit);
    };

    auto make_list = [](auto length, const auto& x = unit)
    {
      auto result { unit };

      for (auto i { 0 }; i < length; ++i)
      {
        result = cons(x, result);
      }

      return result;
    };

    auto xcons = [](auto&&... xs)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return (... | xs);
    };
  } // inline namespace constructor

  /* ==== Predicates ===========================================================
   *
   * From SRFI-1
   *   - circular-list?
   *   - dotted-list?
   *   - eq?                            => eq, object::operator ==
   *   - eqv?                           => eqv, object::compare
   *   - euqal?                         => equal
   *   - list?
   *   - not-pair?                      => not x.is<pair>()
   *   - null-list?
   *   - null?                          => object::operator bool
   *   - pair?                          => object::is<pair>
   *   - proper-list?
   *
   * ======================================================================== */
  inline namespace predicate
  {
    auto null = [](auto&& x)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return not x;
    };

    auto eq = [](auto&& x, auto&& y)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return x == y;
    };

    auto eqv = [](auto&& x, auto&& y)
    {
      return x.compare(y);
    };

    bool equal(const object& x, const object& y)
    {
      if (null(x) and null(y))
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

    #if __cpp_nontype_template_parameter_auto
    template <auto Coarseness = 0>
    #else
    template <int Coarseness = 0>
    #endif // __cpp_nontype_template_parameter_auto
    struct equivalence_comparator;

    #define SPECIALIZE_EQUIVALENCE_COMPARATOR(COARSENESS, COMPARE)             \
    template <>                                                                \
    struct equivalence_comparator<COARSENESS>                                  \
    {                                                                          \
      Define_Const_Perfect_Forwarding(operator (), COMPARE);                   \
    }

    SPECIALIZE_EQUIVALENCE_COMPARATOR(0, eq);
    SPECIALIZE_EQUIVALENCE_COMPARATOR(1, eqv);
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
   * ======================================================================== */
  inline namespace selector
  {
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

    auto list_tail = [](auto&& list, auto&& k)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return std::next(std::begin(list), k);
    };

    auto list_reference = [](auto&&... xs)
    {
      return car(list_tail(std::forward<decltype(xs)>(xs)...));
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
   * From SRFI-1
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
   * ======================================================================== */
  inline namespace miscellaneous
  {
    auto length = [](const auto& x)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return std::distance(std::begin(x), std::end(x));
    };

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

    auto reverse(const object& x) -> object
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
   * From SRFI-1
   *   - fold
   *   - fold-right
   *   - pair-fold
   *   - pair-fold-right
   *   - reduce
   *   - reduce-right
   *
   * ======================================================================== */
  inline namespace folding
  {
  }

  /* ==== Unfolding ============================================================
   *
   * From SRFI-1
   *   - unfold
   *   - unfold-right
   *
   * ======================================================================== */
  inline namespace unfolding
  {
  }

  /* ==== Mapping ==============================================================
   *
   * From SRFI-1
   *   - append-map
   *   - append-map!
   *   - filter-map
   *   - for-each
   *   - map
   *   - map!
   *   - map-in-order
   *   - pair-for-each
   *
   * ======================================================================== */
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

  /* ==== Filtering & partitioning =============================================
   *
   * From SRFI-1
   *   - filter
   *   - filter!
   *   - partition
   *   - partition!
   *   - remove
   *   - remove!
   *
   * ======================================================================== */

  /* ==== Searching ============================================================
   *
   * From SRFI-1
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
   * ======================================================================== */
  inline namespace searching
  {
    auto find = [](const auto& list, auto&& predicate)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      const auto result { std::find_if(std::begin(list), std::end(list), predicate) };
      return result ? car(result) : f;
    };
  }

  /* ==== Deletion =============================================================
   *
   * From SRFI-1
   *   - delete
   *   - delete!
   *   - delete-duplicates
   *   - delete-duplicates!
   *
   * ======================================================================== */

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
   * ======================================================================== */
  inline namespace association_list
  {
    auto assoc = [](const auto& key, const auto& alist, auto&& compare = equivalence_comparator<2>())
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return find(alist, [&](auto&& each) { return compare(car(each), key); });
    };

    auto assv = [](auto&&... xs)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return assoc(std::forward<decltype(xs)>(xs)..., equivalence_comparator<1>());
    };

    auto assq = [](auto&&... xs)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return assoc(std::forward<decltype(xs)>(xs)..., equivalence_comparator<0>());
    };

    auto alist_cons = [](auto&& key, auto&& datum, auto&& alist)
      #if 201603 <= __cpp_constexpr
      constexpr
      #endif
    {
      return cons(cons(key, datum), alist);
    };
  }

  /* ==== Set operations on lists ==============================================
   *
   * From SRFI-1
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
   * ======================================================================= */


  /* ==== Primitive side-effects ===============================================
   *
   * From SRFI-1
   *   - set-car!                         ... TODO
   *   - set-cdr!                         ... TODO
   *
   * ======================================================================== */
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP

