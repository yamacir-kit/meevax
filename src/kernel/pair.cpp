/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/kernel/circular_list.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  collector::mutator::mutator(std::nullptr_t) noexcept
  {}

  collector::mutator::mutator(mutator const& other)
    : nan_boxing_pointer { other }
  {
    if (*this)
    {
      assert(not mutators().contains(this));
      mutators().insert(this);
    }
  }

  collector::mutator::mutator(pair * pair)
    : nan_boxing_pointer { pair }
  {
    if (pair)
    {
      assert(not mutators().contains(this));
      mutators().insert(this);
    }
  }

  collector::mutator::~mutator()
  {
    if (nan_boxing_pointer::operator bool() and not cleared())
    {
      assert(mutators().contains(this));
      mutators().erase(this);
    }
  }

  auto collector::mutator::operator =(mutator const& other) -> mutator &
  {
    reset(other);
    return *this;
  }

  auto collector::mutator::operator =(std::nullptr_t) -> mutator &
  {
    reset();
    return *this;
  }

  auto collector::mutator::eqv(mutator const& rhs) const -> bool
  {
    if (nan_boxing_pointer::dereferenceable())
    {
      return *this ? nan_boxing_pointer::unsafe_get()->eqv(rhs.get()) : rhs.is<std::nullptr_t>();
    }
    else
    {
      return nan_boxing_pointer::compare(rhs);
    }
  }

  auto collector::mutator::reset(mutator const& after) -> void
  {
    auto const before = nan_boxing_pointer::operator bool();

    nan_boxing_pointer::reset(after);

    if (before)
    {
      if (not after)
      {
        assert(mutators().contains(this));
        mutators().erase(this);
      }
    }
    else if (after)
    {
      assert(not mutators().contains(this));
      mutators().insert(this);
    }
  }

  auto collector::mutator::reset(std::nullptr_t) -> void
  {
    auto const before = nan_boxing_pointer::operator bool();

    nan_boxing_pointer::reset();

    if (before)
    {
      assert(mutators().contains(this));
      mutators().erase(this);
    }
  }

  auto collector::mutator::type() const -> std::type_info const&
  {
    if (nan_boxing_pointer::dereferenceable())
    {
      return *this ? nan_boxing_pointer::unsafe_get()->type() : typeid(std::nullptr_t);
    }
    else
    {
      return nan_boxing_pointer::type();
    }
  }

  auto collector::mutator::write(std::ostream & os) const -> std::ostream &
  {
    if (nan_boxing_pointer::dereferenceable())
    {
      return *this ? nan_boxing_pointer::unsafe_get()->write(os) : os << magenta("()");
    }
    else
    {
      return nan_boxing_pointer::write(os);
    }
  }

  auto collector::pair::eqv(pair const* x) const -> bool
  {
    return static_cast<pair const*>(this) == x and static_cast<pair const&>(*this) == *x;
  }

  auto collector::pair::extent() const noexcept -> std::pair<void const*, std::size_t>
  {
    return { static_cast<pair const*>(this), sizeof(pair) };
  }

  auto collector::pair::contains(void const* p) const noexcept -> bool
  {
    auto base = static_cast<pair const*>(this);
    return base <= p and p < reinterpret_cast<void const*>(reinterpret_cast<std::uintptr_t>(base) + sizeof(pair));
  }

  auto collector::pair::type() const noexcept -> std::type_info const&
  {
    return typeid(pair);
  }

  auto collector::pair::write(std::ostream & o) const -> std::ostream &
  {
    return o << static_cast<pair const&>(*this);
  }

  let unit = nullptr;

  struct datum_labels
  {
    std::ostream & os;

    auto static inline count_index = std::ios_base::xalloc();

    auto static inline table_index = std::ios_base::xalloc();

    using table = std::unordered_map<pair const*, std::size_t>;

    explicit datum_labels(std::ostream & os)
      : os { os }
    {
      if (not os.iword(count_index)++)
      {
        os.pword(table_index) = new table();
      }
    }

    ~datum_labels()
    {
      if (not --os.iword(count_index))
      {
        delete static_cast<table *>(std::exchange(os.pword(table_index), nullptr));
      }
    }

    auto static of(std::ostream & os)
    {
      return static_cast<table *>(os.pword(table_index));
    }
  };

  auto operator <<(std::ostream & os, pair const& datum) -> std::ostream &
  {
    auto _ = datum_labels(os);

    if (is_circular_list(cdr(datum)))
    {
      if (auto [iterator, success] = datum_labels::of(os)->emplace(&datum, datum_labels::of(os)->size() + 1); success)
      {
        os << magenta("#", iterator->second, "=(");

        for (auto&& x : datum | as_circular_list)
        {
          os << x << " ";
        }

        return os << magenta(". #", iterator->second, "#)");
      }
      else
      {
        return os << magenta("#", iterator->second, "#");
      }
    }
    else
    {
      os << magenta("(") << car(datum);

      for (let xs = cdr(datum); not xs.is<null>(); xs = cdr(xs))
      {
        if (xs.is<pair>())
        {
          os << " " << car(xs);
        }
        else // xs is the last element of dotted-list.
        {
          return os << magenta(" . ") << xs << magenta(")");
        }
      }

      return os << magenta(")");
    }
  }
} // namespace meevax::kernel
