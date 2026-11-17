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
#include <meevax/memory/multichannel_pointer_set.hpp>
#include <functional>
#include <sstream>
#include <vector>

namespace meevax::inline kernel
{
  auto constinit size = std::size_t(0_MiB);

  enum class extent : std::uintptr_t {};

  auto constinit directory = multichannel_pointer_set<pair, object, extent>();

  static_assert(std::is_trivially_destructible_v<decltype(directory)>);

  auto constinit capacity = std::size_t(16_MiB);

  auto constinit anchor_count = std::size_t();

  anchor::anchor() noexcept
  {
    ++anchor_count;
  }

  anchor::~anchor() noexcept
  {
    assert(anchor_count);

    if (not --anchor_count)
    {
      directory.clear();
    }
  }

  object::object(std::nullptr_t) noexcept
  {}

  object::object(object const& other) noexcept
    : pointer { other }
  {
    if (*this)
    {
      directory.insert(this);
    }
  }

  object::object(pair * pair) noexcept
    : pointer { pair }
  {
    assert(*this);
    directory.insert(this);
  }

  object::~object() noexcept
  {
    if (*this)
    {
      directory.erase(this);
    }
  }

  auto object::operator =(object const& other) noexcept -> object &
  {
    reset(other);
    return *this;
  }

  [[gnu::flatten]]
  auto object::operator =(std::tuple<object const&, object const&> tuple) -> object &
  {
    request(sizeof(pair));

    if (auto datum = new binder<pair, segregated_storage_allocator<void>>(std::get<0>(tuple), std::get<1>(tuple)); datum)
    {
      insert(datum, datum, sizeof(*datum));

      reset(pointer(datum));

      return *this;
    }
    else [[unlikely]]
    {
      throw std::bad_alloc();
    }
  }

  auto object::operator =(std::nullptr_t) noexcept -> object &
  {
    reset();
    return *this;
  }

  auto object::eqv(object const& rhs) const noexcept -> bool
  {
    return *this ? pointer::unsafe_get()->eqv(rhs.get()) : static_cast<pointer const&>(*this) == static_cast<pointer const&>(rhs);
  }

  auto object::external_representation() const -> std::string
  {
    auto oss = std::ostringstream();
    write(oss);
    return oss.str();
  }

  auto object::reset(pointer const& x) noexcept -> void
  {
    if (*this)
    {
      pointer::reset(x);

      if (not *this)
      {
        directory.erase(this);
      }
    }
    else
    {
      pointer::reset(x);

      if (*this)
      {
        directory.insert(this);
      }
    }
  }

  auto object::reset(std::nullptr_t) noexcept -> void
  {
    auto const b = static_cast<bool>(*this);

    pointer::reset();

    if (b)
    {
      directory.erase(this);
    }
  }

  auto object::type() const noexcept -> std::type_info const&
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

  auto clear() noexcept -> void
  {
    for (auto datum : std::ranges::subrange(directory.begin<pair>(), directory.end()))
    {
      auto const [base, size] = datum->extent();
      delete datum;
      directory.erase(datum);
      directory.erase(static_cast<extent const*>(base), size);
    }
  }

  [[gnu::noinline]]
  auto collect() noexcept -> void
  {
    static constinit decltype(directory) new_directory;

    auto static stack = std::vector<pair const*>();

    auto live_size = std::size_t();

    auto mark = [&](auto address)
    {
      if (auto const* datum = address->unsafe_get(); directory.erase(datum))
      {
        new_directory.insert(datum);
        stack.push_back(datum);
      }
    };

    for (auto address : directory.difference<object, extent>())
    {
      mark(address);

      while (not stack.empty())
      {
        auto const datum = stack.back();

        stack.pop_back();

        auto const [base, size] = datum->extent();

        live_size += size;

        new_directory.insert(static_cast<extent const*>(base), size);

        auto const* last = static_cast<std::byte const*>(base) + size;

        for (auto iterator = directory.lower_bound(static_cast<object const*>(base)); iterator != directory.end() and std::less<void const*>()(*iterator, last); ++iterator)
        {
          mark(*iterator);
        }
      }
    }

    size = live_size;

    for (auto datum : std::ranges::subrange(directory.begin<pair>(), directory.end()))
    {
      delete datum;
    }

    for (auto each : std::ranges::subrange(directory.begin<object>(), directory.end()))
    {
      new_directory.insert(each);
    }

    directory.swap(new_directory);

    new_directory.clear();

    capacity = std::max(capacity, size + (size / 2));
  }

  auto count() noexcept -> std::size_t
  {
    return std::ranges::distance(directory.begin<pair>(), directory.end());
  }

  auto insert(pair const* datum, void const* base, std::size_t size) noexcept -> void
  {
    directory.insert(datum);
    directory.insert(static_cast<extent const*>(base), size);
  }

  auto request(std::size_t n) noexcept -> void
  {
    if (size += n; capacity < size)
    {
      collect();
    }
  }

  auto reserve(std::size_t n) noexcept -> void
  {
    capacity = n;
  }

  status::status()
  {
    for (auto i = directory.begin<object>(); i != directory.end(); ++i)
    {
      auto const root = not i.contains<extent>();
      ++(root ? root_count : non_root_count);
      ++(root ? root_count_of : non_root_count_of)[(*i)->type()];
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
