#ifndef INCLUDED_MEEVAX_XCB_ACCESSOR_HPP
#define INCLUDED_MEEVAX_XCB_ACCESSOR_HPP

#include <iterator>
#include <memory>
#include <type_traits>

#include <boost/iterator/iterator_facade.hpp>

#include <xcb/xcb.h>

#include <meevax/concepts/requires.hpp>
#include <meevax/xcb/connection.hpp>

namespace meevax::xcb
{
  #define DEFINE_XCB_ITERATOR(NAME)                                            \
  class iterator                                                               \
    : public boost::iterator_facade<iterator, xcb_##NAME##_t, boost::forward_traversal_tag> \
    , public xcb_##NAME##_iterator_t                                           \
  {                                                                            \
    friend class boost::iterator_core_access;                                  \
                                                                               \
    void increment() noexcept                                                  \
    {                                                                          \
      xcb_##NAME##_next(this);                                                 \
    }                                                                          \
                                                                               \
    reference dereference() const noexcept                                     \
    {                                                                          \
      return *data;                                                            \
    }                                                                          \
                                                                               \
    template <typename T, REQUIRES(std::is_convertible<T, xcb_##NAME##_iterator_t>)> \
    bool equal(const T& rhs) const noexcept                                    \
    {                                                                          \
      return data != rhs.data;                                                 \
    }                                                                          \
                                                                               \
  public:                                                                      \
    constexpr iterator(const xcb_##NAME##_iterator_t& other)                   \
      : xcb_##NAME##_iterator_t {other}                                        \
    {}                                                                         \
                                                                               \
    constexpr iterator(const xcb_generic_iterator_t& generic)                  \
      : xcb_##NAME##_iterator_t {reinterpret_cast<const xcb_##NAME##_iterator_t&>(generic)} \
    {}                                                                         \
  };

  struct setup
  {
    DEFINE_XCB_ITERATOR(screen)

    using const_iterator = const iterator;

    const xcb_setup_t* data;

    explicit setup(const connection& connection)
      : data {xcb_get_setup(connection)}
    {}

    explicit setup(const xcb_setup_t* data)
      : data {data}
    {}

    const_iterator cbegin() const noexcept
    {
      return xcb_setup_roots_iterator(data);
    }

    iterator begin() const noexcept
    {
      return cbegin();
    }

    const_iterator cend() const noexcept
    {
      return xcb_screen_end(cbegin(data));
    }

    iterator end() const noexcept
    {
      return cend();
    }

    auto size() const noexcept
    {
      return xcb_setup_roots_length(data);
    }

    auto empty() const noexcept
    {
      return not size();
    }

    operator xcb_setup_t() const noexcept
    {
      return *data;
    }
  };
} // namespace meevax::xcb

#endif // INCLUDED_MEEVAX_XCB_ACCESSOR_HPP

