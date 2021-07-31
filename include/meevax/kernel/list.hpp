/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <algorithm>
#include <numeric>

#include <meevax/functional/combinator.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/iterator.hpp>

/* ---- Procedure Index --------------------------------------------------------
 *
 *  Constructors
 *    cons list
 *    xcons cons* make-list list-tabulate
 *    list-copy circular-list iota
 *
 *  Predicates
 *    pair? null?
 *    proper-list? circular-list? dotted-list?
 *    not-pair? null-list?
 *    list=
 *
 *  Selectors
 *    car cdr ... cddadr cddddr list-ref
 *    first second third fourth fifth sixth seventh eighth ninth tenth
 *    car+cdr
 *    take       drop
 *    take-right drop-right
 *    take!      drop-right!
 *    split-at   split-at!
 *    last last-pair
 *
 *  Miscellaneous: length, append, concatenate, reverse, zip & count
 *    length length+
 *    append  concatenate  reverse
 *    append! concatenate! reverse!
 *    append-reverse append-reverse!
 *    zip unzip1 unzip2 unzip3 unzip4 unzip5
 *    count
 *
 *  Fold, unfold & map
 *    map for-each
 *    fold       unfold       pair-fold       reduce
 *    fold-right unfold-right pair-fold-right reduce-right
 *    append-map append-map!
 *    map! pair-for-each filter-map map-in-order
 *
 *  Filtering & partitioning
 *    filter  partition  remove
 *    filter! partition! remove!
 *
 *  Searching
 *    member memq memv
 *    find find-tail
 *    any every
 *    list-index
 *    take-while drop-while take-while!
 *    span break span! break!
 *
 *  Deleting
 *    delete  delete-duplicates
 *    delete! delete-duplicates!
 *
 *  Association lists
 *    assoc assq assv
 *    alist-cons alist-copy
 *    alist-delete alist-delete!
 *
 *  Set operations on lists
 *    lset<= lset= lset-adjoin
 *    lset-union             lset-union!
 *    lset-intersection      lset-intersection!
 *    lset-difference        lset-difference!
 *    lset-xor               lset-xor!
 *    lset-diff+intersection lset-diff+intersection!
 *
 *  Primitive side-effects
 *    set-car! set-cdr!
 *
 * -------------------------------------------------------------------------- */

namespace meevax
{
inline namespace kernel
{
  template <typename T, typename U, REQUIRES(std::is_convertible<T, let>,
                                             std::is_convertible<U, let>)>
  auto operator |(T&& x, U&& y) -> decltype(auto)
  {
    return make<pair>(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
  }

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  cons a d -> pair
   *
   *    The primitive constructor. Returns a newly allocated pair whose car is a
   *    and whose cdr is d. The pair is guaranteed to be different (in the sense
   *    of eqv?) from every existing object.
   *
   *
   *  cons* elt1 elt2 ... -> object
   *
   *    Like list, but the last argument provides the tail of the constructed
   *    list, returning
   *
   *      (cons elt1 (cons elt2 (cons ... eltn)))
   *
   *    This function is called list* in Common Lisp and about half of the
   *    Schemes that provide it, and cons* in the other half.
   *
   * ------------------------------------------------------------------------ */
  auto cons = [](auto&&... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ...);
  };

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  list object ... -> list
   *
   *    Returns a newly allocated list of its arguments.
   *
   * ------------------------------------------------------------------------ */
  auto list = [](auto&& ... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ... | unit);
  };

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  xcons d a -> pair
   *
   *    Of utility only as a value to be conveniently passed to higher-order
   *    procedures. The name stands for "eXchanged CONS."
   *
   * ------------------------------------------------------------------------ */
  auto xcons = [](auto&& d, auto&& a) constexpr
  {
    return cons(std::forward<decltype(a)>(a), std::forward<decltype(d)>(d));
  };

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  make-list n [fill] -> list
   *
   *    Returns an n-element list, whose elements are all the value fill. If
   *    the fill argument is not given, the elements of the list may be
   *    arbitrary values.
   *
   * ------------------------------------------------------------------------ */
  auto make_list = [](std::size_t k, let const& x = unit)
  {
    let result = list();

    for (std::size_t i = 0; i < k; ++i)
    {
      result = cons(x, result);
    }

    return result;
  };

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  list-tabulate n init-proc -> list
   *
   *    Returns an n-element list. Element i of the list, where 0 <= i < n, is
   *    produced by (init-proc i). No guarantee is made about the dynamic order
   *    in which init-proc is applied to these indices.
   *
   * ------------------------------------------------------------------------ */
  auto list_tabulate = [](auto n, auto&& initialize)
  {
    let x = list();

    while (0 <= --n)
    {
      x = cons(initialize(n), x);
    }

    return x;
  };

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  list-copy flist -> flist
   *
   *    Copies the spine of the argument.
   *
   * ------------------------------------------------------------------------ */
  auto list_copy = [](auto const& x)
  {
    auto copy = [](auto&& rec, let const& x) -> let
    {
      if (x.is<pair>())
      {
        return cons(car(x), rec(rec, cdr(x)));
      }
      else
      {
        return x;
      }
    };

    return z(copy)(x);
  };

  /* ---- NOTE -----------------------------------------------------------------
   *
   *  circular-list elt1 elt2 ... -> list
   *
   *    Constructs a circular list of the elements.
   *
   * ------------------------------------------------------------------------ */
  auto circular_list = [](auto&&... xs)
  {
    let x = list(std::forward<decltype(xs)>(xs)...);

    if (auto const length = std::distance(std::cbegin(x), std::cend(x)); 0 < length)
    {
      cdr(std::next(std::begin(x), length - 1)) = x;
    }

    return x;
  };

  /* ---- Predicates -----------------------------------------------------------
   *
   * From SRFI-1
   *   - circular-list?
   *   - dotted-list?
   *   - eq?                            => eq, let::operator ==
   *   - eqv?                           => eqv, let::eqv
   *   - euqal?                         => equal
   *   - list?
   *   - not-pair?                      => not x.is<pair>()
   *   - null-list?
   *   - null?                          => let::is<null>()
   *   - pair?                          => let::is<pair>()
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

  auto equal(let const& x, let const& y) -> bool;

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
    decltype(auto) list_tail(T&& x, let const& k)
    {
      return list_tail(std::forward<decltype(x)>(x), k.as<exact_integer>().to<std::size_t>());
    }

    template <typename... Ts>
    constexpr decltype(auto) list_ref(Ts&&... xs)
    {
      return car(list_tail(std::forward<decltype(xs)>(xs)...));
    }

    let take(let const& exp, std::size_t size);
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

    let append(let const& x, let const& y);

    let reverse(let const& x);

    let zip(let const& x, let const& y);
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
    let map(Procedure&& procedure, let const& x)
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
    auto find = [](let const& x, auto&& predicate) constexpr -> let const&
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
    auto assoc = [](let const& key, let const& alist, auto&& compare = equivalence_comparator<2>()) constexpr
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

