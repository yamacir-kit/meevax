#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <algorithm> // std::equal

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/iterator.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Constructors ---------------------------------------------------------
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
   * ------------------------------------------------------------------------ */
  template <typename T, typename U,
            REQUIRES(std::is_convertible<T, object>,
                     std::is_convertible<U, object>)>
  inline decltype(auto) operator |(T&& x, U&& y)
  {
    return make<pair>(std::forward<decltype(x)>(x),
                      std::forward<decltype(y)>(y));
  }

  auto cons = [](auto&&... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ...);
  };

  auto list = [](auto&& ... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ... | unit);
  };

  auto make_list = [](auto length, let const& x = unit)
  {
    let result = unit;

    for (decltype(length) i = 0; i < length; ++i)
    {
      result = cons(x, result);
    }

    return result;
  };

  auto xcons = [](auto&&... xs) constexpr
  {
    return (... | std::forward<decltype(xs)>(xs));
  };

  /* ---- Predicates -----------------------------------------------------------
   *
   * From SRFI-1
   *   - circular-list?
   *   - dotted-list?
   *   - eq?                            => eq, object::operator ==
   *   - eqv?                           => eqv, object::eqv
   *   - euqal?                         => equal
   *   - list?
   *   - not-pair?                      => not x.is<pair>()
   *   - null-list?
   *   - null?                          => object::is<null>()
   *   - pair?                          => object::is<pair>()
   *   - proper-list?
   *
   * ------------------------------------------------------------------------ */

  auto eq = [](auto const& x, auto const& y) constexpr
  {
    return x == y;
  };

  auto eqv = [](auto const& x, auto const& y)
  {
    return x.eqv(y);
  };

  auto equal(object const& x, object const& y) -> bool;

  template <std::size_t Coarseness = 0>
  struct equivalence_comparator;

  #define SPECIALIZE_EQUIVALENCE_COMPARATOR(COARSENESS, COMPARE)               \
  template <>                                                                  \
  struct equivalence_comparator<COARSENESS>                                    \
  {                                                                            \
    template <typename... Ts>                                                  \
    constexpr decltype(auto) operator ()(Ts&&... xs) const                     \
    {                                                                          \
      return COMPARE(std::forward<decltype(xs)>(xs)...);                       \
    }                                                                          \
  }

  SPECIALIZE_EQUIVALENCE_COMPARATOR(0, eq);
  SPECIALIZE_EQUIVALENCE_COMPARATOR(1, eqv);
  SPECIALIZE_EQUIVALENCE_COMPARATOR(2, equal);

  #undef SPECIALIZE_EQUIVALENCE_COMPARATOR

  using default_equivalence_comparator = equivalence_comparator<>;

  /* ---- Selectors ------------------------------------------------------------
   *
   * From R7RS
   *   - car                              => car
   *   - cdr                              => cdr
   *   - cxr
   *   - list-ref                         => list_ref
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
   * ------------------------------------------------------------------------ */
  inline namespace selector
  {
    constexpr auto caar = compose(car, car);
    constexpr auto cadr = compose(car, cdr);
    constexpr auto cdar = compose(cdr, car);
    constexpr auto cddr = compose(cdr, cdr);

    constexpr auto caaar = compose(car, caar);
    constexpr auto caadr = compose(car, cadr);
    constexpr auto cadar = compose(car, cdar);
    constexpr auto caddr = compose(car, cddr);
    constexpr auto cdaar = compose(cdr, caar);
    constexpr auto cdadr = compose(cdr, cadr);
    constexpr auto cddar = compose(cdr, cdar);
    constexpr auto cdddr = compose(cdr, cddr);

    constexpr auto caaaar = compose(car, caaar);
    constexpr auto caaadr = compose(car, caadr);
    constexpr auto caadar = compose(car, cadar);
    constexpr auto caaddr = compose(car, caddr);
    constexpr auto cadaar = compose(car, cdaar);
    constexpr auto cadadr = compose(car, cdadr);
    constexpr auto caddar = compose(car, cddar);
    constexpr auto cadddr = compose(car, cdddr);
    constexpr auto cdaaar = compose(cdr, caaar);
    constexpr auto cdaadr = compose(cdr, caadr);
    constexpr auto cdadar = compose(cdr, cadar);
    constexpr auto cdaddr = compose(cdr, caddr);
    constexpr auto cddaar = compose(cdr, cdaar);
    constexpr auto cddadr = compose(cdr, cdadr);
    constexpr auto cdddar = compose(cdr, cddar);
    constexpr auto cddddr = compose(cdr, cdddr);

    template <typename T>
    constexpr decltype(auto) list_tail(T&& x, std::size_t k)
    {
      return std::next(std::cbegin(std::forward<decltype(x)>(x)), k);
    }

    template <typename T>
    decltype(auto) list_tail(T&& x, object const& k)
    {
      return list_tail(std::forward<decltype(x)>(x), k.as<exact_integer>().to<std::size_t>());
    }

    template <typename... Ts>
    constexpr decltype(auto) list_ref(Ts&&... xs)
    {
      return car(list_tail(std::forward<decltype(xs)>(xs)...));
    }

    let take(const object& exp, std::size_t size);
  }

  /* ---- Miscellaneous --------------------------------------------------------
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
   * ------------------------------------------------------------------------ */
  inline namespace miscellaneous
  {
    auto length = [](auto const& x) constexpr
    {
      return std::distance(std::cbegin(x), std::cend(x));
    };

    let append(const object& x, const object& y);

    let reverse(const object& x);

    let zip(const object& x, const object& y);
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
    object map(Procedure&& procedure, const object& x)
    {
      if (x.is<null>())
      {
        return unit;
      }
      else
      {
        return cons(procedure(car(x)), map(procedure, cdr(x)));
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
    auto find = [](auto const& x, auto&& predicate) constexpr -> auto const&
    {
      if (auto const& iter = std::find_if(std::cbegin(x), std::cend(x), std::forward<decltype(predicate)>(predicate)); iter)
      {
        return *iter;
      }
      else
      {
        return f;
      }
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
    auto assoc = [](auto const& key, auto const& alist, auto&& compare = equivalence_comparator<2>()) constexpr
    {
      return find(alist, [&](auto&& each)
             {
               return compare(car(each), key);
             });
    };

    auto assv = [](auto&&... xs) constexpr
    {
      return assoc(std::forward<decltype(xs)>(xs)..., equivalence_comparator<1>());
    };

    auto assq = [](auto&&... xs) constexpr
    {
      return assoc(std::forward<decltype(xs)>(xs)..., equivalence_comparator<0>());
    };

    auto alist_cons = [](auto&& key, auto&& datum, auto&& alist) constexpr
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
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP

