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
#include <meevax/memory/pointer_set.hpp>
#include <vector>

namespace meevax::inline kernel
{
  /*
     https://www.kernel.org/doc/html/latest/arch/x86/x86_64/mm.html

     0x0000'0000'0000'0000 ~ 0x0000'7FFF'FFFF'FFFF
  */
  template <typename T>
  using canonical_pointer_set = pointer_set<T const*, std::bit_width(0x7FFFu), std::bit_width(0xFFFFu), std::bit_width(0xFFFFu)>;

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
      insert();
    }
  }

  object::object(pair * pair)
    : pointer { pair }
  {
    if (pair)
    {
      insert();
    }
  }

  object::~object()
  {
    if (*this and not cleared)
    {
      erase();
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
    return *this ? pointer::unsafe_get()->eqv(rhs.get()) : static_cast<pointer const&>(*this) == static_cast<pointer const&>(rhs);
  }

  auto object::erase() const -> void
  {
    assert(objects.contains(this));
    objects.erase(this);
  }

  auto object::insert() const -> void
  {
    assert(not objects.contains(this));
    objects.insert(this);
  }

  auto object::type() const -> std::type_info const&
  {
    return *this ? pointer::unsafe_get()->type() : pointer::type();
  }

  auto object::write(std::ostream & os) const -> std::ostream &
  {
    return *this ? pointer::unsafe_get()->write(os) : pointer::write(os);
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

    auto static roots = std::vector<object const*>();

    roots.clear();

    for (auto x : objects)
    {
      if (is_root(x))
      {
        roots.push_back(x);
      }
    }

    size = 0;

    auto new_data = canonical_pointer_set<pair>();

    for (auto root : roots)
    {
      auto static stack = std::vector<pair const*>();

      auto mark = [&](auto m)
      {
        assert(m);
        assert(m->unsafe_get());

        if (auto datum = m->unsafe_get(); not new_data.contains(datum))
        {
          new_data.insert(datum);
          data.erase(datum);
          stack.push_back(datum);
        }
      };

      mark(root);

      while (not stack.empty())
      {
        auto [base, size_] = stack.back()->extent();

        size += size_;

        stack.pop_back();

        std::for_each(objects.lower_bound(reinterpret_cast<object const*>(base)),
                      objects.lower_bound(reinterpret_cast<object const*>(reinterpret_cast<std::uintptr_t>(base) + size_)),
                      mark);
      }
    }

    for (auto datum : data)
    {
      delete datum;
    }

    data.swap(new_data);

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

    auto contains = [&]()
    {
      auto const [base, size] = (*iterator)->extent();
      return reinterpret_cast<std::uintptr_t>(x) - reinterpret_cast<std::uintptr_t>(base) < size; // NOTE: Same as base <= x and x < base + size
    };

    return not ((iterator and contains()) or (--iterator and contains()));
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
} // namespace meevax::kernel
