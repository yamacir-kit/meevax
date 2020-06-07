#ifndef INCLUDED_MEEVAX_PROTOCOL_ACCESSOR_HPP
#define INCLUDED_MEEVAX_PROTOCOL_ACCESSOR_HPP

#include <iterator>
#include <memory>
#include <type_traits>

#include <boost/iterator/iterator_facade.hpp>

#include <xcb/xcb.h>

#include <meevax/concepts/requires.hpp>
#include <meevax/protocol/connection.hpp>

namespace meevax::protocol
{
  #define DEFINE_PROTOCOL_ITERATOR(NAME)                                       \
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
    template <typename T, Requires(std::is_convertible<T, xcb_##NAME##_iterator_t>)> \
    bool equal(const T& rhs) const noexcept                                    \
    {                                                                          \
      return data == rhs.data;                                                 \
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

  #define DEFINE_PROTOCOL_ACCESSOR(NAME, ITERATOR, SUFFIX, ...)                \
  struct NAME                                                                  \
  {                                                                            \
    DEFINE_PROTOCOL_ITERATOR(ITERATOR)                                         \
                                                                               \
    using const_iterator = const iterator;                                     \
                                                                               \
    const xcb_##NAME##_t* data;                                                \
                                                                               \
    explicit NAME(const xcb_##NAME##_t* data)                                  \
      : data {data}                                                            \
    {}                                                                         \
                                                                               \
    const_iterator cbegin() const noexcept                                     \
    {                                                                          \
      return xcb_##NAME##_##SUFFIX##_iterator(data);                           \
    }                                                                          \
                                                                               \
    iterator begin() const noexcept                                            \
    {                                                                          \
      return cbegin();                                                         \
    }                                                                          \
                                                                               \
    const_iterator cend() const noexcept                                       \
    {                                                                          \
      return xcb_##ITERATOR##_end(                                             \
               xcb_##NAME##_##SUFFIX##_iterator(data)                          \
             );                                                                \
    }                                                                          \
                                                                               \
    iterator end() const noexcept                                              \
    {                                                                          \
      return cend();                                                           \
    }                                                                          \
                                                                               \
    auto size() const noexcept                                                 \
    {                                                                          \
      return xcb_##NAME##_##SUFFIX##_length(data);                             \
    }                                                                          \
                                                                               \
    auto empty() const noexcept                                                \
    {                                                                          \
      return not size();                                                       \
    }                                                                          \
                                                                               \
    operator xcb_##NAME##_t() const noexcept                                   \
    {                                                                          \
      return *data;                                                            \
    }                                                                          \
                                                                               \
    __VA_ARGS__                                                                \
  };

  DEFINE_PROTOCOL_ACCESSOR(setup, screen, roots,
    explicit setup(const connection& connection)
      : data {xcb_get_setup(connection)}
    {}
  )

  auto root_screen(const connection& connection)
  {
    return setup {connection}.begin()->root;
  }

  DEFINE_PROTOCOL_ACCESSOR(screen, depth, allowed_depths, )

  DEFINE_PROTOCOL_ACCESSOR(depth, visualtype, visuals, )

  auto root_visualtype(const connection& connection)
  {
    for (const auto& each_screen : setup {connection})
    {
      for (const auto& each_depth : screen {&each_screen})
      {
        for (auto&& each_visualtype : depth {&each_depth})
        {
          if (each_screen.root_visual == each_visualtype.visual_id)
          {
            return &each_visualtype;
          }
        }
      }
    }

    throw std::runtime_error {"ROOT_VISUALTYPE NOT FOUND"};
  }
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_ACCESSOR_HPP

