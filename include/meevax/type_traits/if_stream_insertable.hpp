#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IF_STREAM_INSERTABLE_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IF_STREAM_INSERTABLE_HPP

#include <meevax/console/escape_sequence.hpp>
#include <meevax/type_traits/is_stream_insertable.hpp>

namespace meevax { inline namespace type_traits
{
  template <typename U, typename = void>
  struct if_stream_insertable
  {
    static auto call_it(std::ostream& port, const U& rhs) -> decltype(auto)
    {
      return port << magenta << "#,(" << green << typeid(U).name() << reset << " " << static_cast<const U*>(&rhs) << magenta << ")" << reset;
    }
  };

  template <typename U>
  struct if_stream_insertable<U, typename std::enable_if<is_stream_insertable<U>::value>::type>
  {
    static auto call_it(std::ostream& port, const U& rhs) -> decltype(auto)
    {
      return port << rhs;
    }
  };
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IF_STREAM_INSERTABLE_HPP
