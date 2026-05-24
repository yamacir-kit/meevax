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

#include <meevax/kernel/collector.hpp>

namespace meevax::inline kernel
{
  mutator::mutator(std::nullptr_t) noexcept
  {}

  mutator::mutator(mutator const& other)
    : pointer { other }
  {
    if (*this)
    {
      assert(not mutators().contains(this));
      mutators().insert(this);
    }
  }

  mutator::mutator(pair * pair)
    : pointer { pair }
  {
    if (pair)
    {
      assert(not mutators().contains(this));
      mutators().insert(this);
    }
  }

  mutator::~mutator()
  {
    if (pointer::operator bool() and not cleared())
    {
      assert(mutators().contains(this));
      mutators().erase(this);
    }
  }

  auto mutator::operator =(mutator const& other) -> mutator &
  {
    reset(other);
    return *this;
  }

  auto mutator::operator =(std::nullptr_t) -> mutator &
  {
    reset();
    return *this;
  }

  auto mutator::eqv(mutator const& rhs) const -> bool
  {
    if (pointer::dereferenceable())
    {
      return *this ? pointer::unsafe_get()->eqv(rhs.get()) : rhs.is<std::nullptr_t>();
    }
    else
    {
      return pointer::compare(rhs);
    }
  }

  auto mutator::reset(mutator const& after) -> void
  {
    auto const before = pointer::operator bool();

    pointer::reset(after);

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

  auto mutator::reset(std::nullptr_t) -> void
  {
    auto const before = pointer::operator bool();

    pointer::reset();

    if (before)
    {
      assert(mutators().contains(this));
      mutators().erase(this);
    }
  }

  auto mutator::type() const -> std::type_info const&
  {
    if (pointer::dereferenceable())
    {
      return *this ? pointer::unsafe_get()->type() : typeid(std::nullptr_t);
    }
    else
    {
      return pointer::type();
    }
  }

  auto mutator::write(std::ostream & os) const -> std::ostream &
  {
    if (pointer::dereferenceable())
    {
      return *this ? pointer::unsafe_get()->write(os) : os << magenta("()");
    }
    else
    {
      return pointer::write(os);
    }
  }

  auto clear() -> void
  {
    for (auto const& object : objects())
    {
      delete object;
      assert(objects().contains(object));
      objects().erase(object);
    }
  }

  auto cleared() -> bool &
  {
    auto static cleared = false;
    return cleared;
  }

  auto collect() -> void
  {
    /*
       This mark-and-sweep garbage collector is based on the `gc_ptr`
       implementation developed and posted to CodeProject by William E. Kempf.

       - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
       - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
    */

    auto roots = canonical_pointer_set<mutator>();

    for (auto m : mutators())
    {
      if (is_root(m))
      {
        roots.insert(m);
      }
    }

    size() = 0;

    auto reachables = canonical_pointer_set<pair>();

    for (auto root : roots)
    {
      auto static stack = std::vector<pair const*>();

      auto mark = [&](auto m)
      {
        assert(m);
        assert(m->unsafe_get());

        if (auto p = m->unsafe_get(); not reachables.contains(p))
        {
          reachables.insert(p);
          objects().erase(p);
          stack.push_back(p);
        }
      };

      mark(root);

      while (not stack.empty())
      {
        auto [base, size_] = stack.back()->extent();

        stack.pop_back();

        size() += size_;

        std::for_each(mutators().lower_bound(reinterpret_cast<mutator const*>(base)),
                      mutators().lower_bound(reinterpret_cast<mutator const*>(reinterpret_cast<std::uintptr_t>(base) + size_)),
                      mark);
      }
    }

    for (auto object : objects())
    {
      delete object;
    }

    objects().swap(reachables);

    threshold() = std::max(threshold(), size() + (size() / 2));
  }

  auto count() -> std::size_t
  {
    return objects().size();
  }

  auto is_root(mutator const* m) noexcept -> bool
  {
    /*
       If the given mutator is a non-root object, then an object containing
       this mutator as a data member exists somewhere in memory.

       Containing the mutator as a data member means that the address of the
       mutator is contained in the interval of the object's base-address ~
       base-address + object-size. The pair is present to keep track of the
       base-address and size of the object needed here.

       The memory layout of the base class pair and Bound of the binder is
       implementation-defined. That is, there is no guarantee that the
       pointer value of pair const* is less than the pointer value of Bound
       const*. Therefore, the iterator returned by lower_bound here points to
       pair const*, which may be an iterator to the object itself, which may
       contain m, or the next iterator of the object, which may contain m.
    */
    auto iterator = objects().lower_bound(reinterpret_cast<pair const*>(m));

    return not ((iterator and (*iterator)->contains(m)) or (--iterator and (*iterator)->contains(m)));
  }

  auto mutators() -> canonical_pointer_set<mutator> &
  {
    auto static mutators = canonical_pointer_set<mutator>();
    return mutators;
  }

  auto objects() -> canonical_pointer_set<pair> &
  {
    auto static objects = canonical_pointer_set<pair>();
    return objects;
  }

  auto size() -> std::size_t &
  {
    auto static size = std::size_t(0_MiB);
    return size;
  }

  auto threshold() -> std::size_t &
  {
    auto static threshold = std::size_t(16_MiB);
    return threshold;
  }
} // namespace meevax::kernel
