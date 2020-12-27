#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <algorithm> // std::equal
#include <functional>
#include <iterator> // std::begin, std::end, std::distance

#include <meevax/functional/compose.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Homoiconic Iterator --------------------------------------------------
   *
   * TODO std::empty
   *
   * ------------------------------------------------------------------------ */
  template <typename T>
  struct homoiconic_iterator
    : public std::reference_wrapper<T>
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = std::reference_wrapper<T>;

    using reference = typename std::add_lvalue_reference<value_type>::type;

    using const_reference = typename std::add_const<reference>::type;

    using pointer = value_type; // homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    using std::reference_wrapper<T>::reference_wrapper;

    homoiconic_iterator(T const& x)
      : std::reference_wrapper<T> { std::cref(x) }
    {}

    operator T&()       noexcept { return std::reference_wrapper<T>::get(); }
    operator T&() const noexcept { return std::reference_wrapper<T>::get(); }

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

    decltype(auto) operator++(int)
    {
      auto result { *this };
      operator ++();
      return std::move(result);
    }

    homoiconic_iterator begin() const noexcept { return *this; }
    homoiconic_iterator   end() const noexcept { return unit; }

    decltype(auto) operator==(homoiconic_iterator const& rhs) const noexcept { return std::reference_wrapper<T>::get() == rhs.get(); }
    decltype(auto) operator!=(homoiconic_iterator const& rhs) const noexcept { return std::reference_wrapper<T>::get() != rhs.get(); }
  };
} // namespace kernel
} // namespace meevax

namespace std
{
  auto cbegin(meevax::object const& x) -> meevax::homoiconic_iterator<meevax::object const>;
  auto  begin(meevax::object const& x) -> meevax::homoiconic_iterator<meevax::object const>;
  auto   cend(meevax::object const&  ) -> meevax::homoiconic_iterator<meevax::object const>;
  auto    end(meevax::object const&  ) -> meevax::homoiconic_iterator<meevax::object const>;
} // namespace std

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

    auto make_list = [](auto length, const auto& x = unit)
    {
      auto result { unit };

      for (auto i { 0 }; i < length; ++i)
      {
        result = cons(x, result);
      }

      return result;
    };

    auto xcons = [](auto&&... xs) constexpr
    {
      return (... | xs);
    };
  } // inline namespace constructor

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
  inline namespace predicate
  {
    auto eq = [](auto const& x, auto const& y) constexpr
    {
      return x == y;
    };

    auto eqv = [](auto const& x, auto const& y)
    {
      return x.eqv(y);
    };

    auto equal(const object& x, const object& y) -> bool;

    template <std::size_t Coarseness = 0>
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

  /* ---- Selectors ------------------------------------------------------------
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
   * ------------------------------------------------------------------------ */
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

    auto list_tail = [](auto&& list, auto&& k) constexpr
    {
      return std::next(std::begin(list), k);
    };

    auto list_reference = [](auto&&... xs)
    {
      return car(list_tail(std::forward<decltype(xs)>(xs)...));
    };

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
    auto length = [](const auto& x) constexpr
    {
      return std::distance(std::begin(x), std::end(x));
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
    object map(Procedure procedure, const object& x)
    {
      if (x.is<null>())
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
    auto find = [](auto const& x, auto&& predicate) constexpr
    {
      if (let const& result = std::find_if(std::cbegin(x), std::cend(x), predicate); result)
      {
        return car(result);
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

