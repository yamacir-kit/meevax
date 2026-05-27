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
#include <meevax/kernel/object.hpp>
#include <vector>

namespace meevax::inline kernel
{
  auto size = std::size_t(0_MiB);

  auto capacity = std::size_t(16_MiB);

  auto data = canonical_pointer_set<pair>();

  auto objects = canonical_pointer_set<object>();

  auto cleared = false;

  object::object(std::nullptr_t) noexcept
  {}

  object::object(object const& other)
    : pointer { other }
  {
    if (*this)
    {
      assert(not objects.contains(this));
      objects.insert(this);
    }
  }

  object::object(pair * pair)
    : pointer { pair }
  {
    if (pair)
    {
      assert(not objects.contains(this));
      objects.insert(this);
    }
  }

  object::~object()
  {
    if (pointer::operator bool() and not cleared)
    {
      assert(objects.contains(this));
      objects.erase(this);
    }
  }

  auto object::operator =(object const& other) -> object &
  {
    reset(other);
    return *this;
  }

  auto object::operator =(std::nullptr_t) -> object &
  {
    reset();
    return *this;
  }

  auto object::eqv(object const& rhs) const -> bool
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

  auto object::reset(object const& after) -> void
  {
    auto const before = pointer::operator bool();

    pointer::reset(after);

    if (before)
    {
      if (not after)
      {
        assert(objects.contains(this));
        objects.erase(this);
      }
    }
    else if (after)
    {
      assert(not objects.contains(this));
      objects.insert(this);
    }
  }

  auto object::reset(std::nullptr_t) -> void
  {
    auto const before = pointer::operator bool();

    pointer::reset();

    if (before)
    {
      assert(objects.contains(this));
      objects.erase(this);
    }
  }

  auto object::type() const -> std::type_info const&
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

  auto object::write(std::ostream & os) const -> std::ostream &
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

  auto operator <<(std::ostream & os, object const& datum) -> std::ostream &
  {
    return datum.write(os);
  }

  auto clear() -> void
  {
    for (auto const& datum : data)
    {
      delete datum;
      assert(data.contains(datum));
      data.erase(datum);
    }
  }

  auto clear_once() -> void
  {
    if (not std::exchange(cleared, true))
    {
      clear();
    }
  }

  auto collect() -> void
  {
    /*
       This mark-and-sweep garbage collector is based on the `gc_ptr`
       implementation developed and posted to CodeProject by William E. Kempf.

       - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
       - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
    */

    auto roots = canonical_pointer_set<object>();

    for (auto x : objects)
    {
      if (is_root(x))
      {
        roots.insert(x);
      }
    }

    size = 0;

    auto reachables = canonical_pointer_set<pair>();

    for (auto root : roots)
    {
      auto static stack = std::vector<pair const*>();

      auto mark = [&](auto m)
      {
        assert(m);
        assert(m->unsafe_get());

        if (auto datum = m->unsafe_get(); not reachables.contains(datum))
        {
          reachables.insert(datum);
          data.erase(datum);
          stack.push_back(datum);
        }
      };

      mark(root);

      while (not stack.empty())
      {
        auto [base, size_] = stack.back()->extent();

        stack.pop_back();

        size += size_;

        std::for_each(objects.lower_bound(reinterpret_cast<object const*>(base)),
                      objects.lower_bound(reinterpret_cast<object const*>(reinterpret_cast<std::uintptr_t>(base) + size_)),
                      mark);
      }
    }

    for (auto datum : data)
    {
      delete datum;
    }

    data.swap(reachables);

    capacity = std::max(capacity, size + (size / 2));
  }

  auto count() -> std::size_t
  {
    return data.size();
  }

  auto insert(pair const* datum) -> void
  {
    data.insert(datum);
  }

  auto is_root(object const* x) noexcept -> bool
  {
    /*
       If the given object is a non-root object, then an object containing
       this object as a data member exists somewhere in memory.

       Containing the object as a data member means that the address of the
       object is contained in the interval of the object's base-address ~
       base-address + object-size. The pair is present to keep track of the
       base-address and size of the object needed here.

       The memory layout of the base class pair and Bound of the binder is
       implementation-defined. That is, there is no guarantee that the
       pointer value of pair const* is less than the pointer value of Bound
       const*. Therefore, the iterator returned by lower_bound here points to
       pair const*, which may be an iterator to the object itself, which may
       contain m, or the next iterator of the object, which may contain m.
    */
    auto iterator = data.lower_bound(reinterpret_cast<pair const*>(x));

    return not ((iterator and (*iterator)->contains(x)) or (--iterator and (*iterator)->contains(x)));
  }

  auto request(std::size_t n) -> void
  {
    if (size += n; capacity < size)
    {
      collect();
    }
  }

  auto reserve(std::size_t n) -> void
  {
    capacity = n;
  }

  status::status()
    : root_count { 0 }
    , non_root_count { 0 }
  {
    for (auto const& x : objects)
    {
      if (is_root(x))
      {
        ++root_count;
        ++root_count_of[x->type()];
      }
      else
      {
        ++non_root_count;
        ++non_root_count_of[x->type()];
      }
    }
  }

  auto operator <<(std::ostream & os, status const& status) -> std::ostream &
  {
    os << "collector.root-count=" << status.root_count << '\n';

    for (auto const& [type, count] : status.root_count_of)
    {
      os << "collector.root-count-of." << demangle(type.name()) << "=" << count << '\n';
    }

    os << "collector.non-root-count=" << status.non_root_count << '\n';

    for (auto const& [type, count] : status.non_root_count_of)
    {
      os << "collector.non-root-count-of." << demangle(type.name()) << "=" << count << '\n';
    }

    return os;
  }

namespace backdoor
{
  auto objects() -> canonical_pointer_set<object> const&
  {
    return kernel::objects;
  }
}
} // namespace meevax::kernel
