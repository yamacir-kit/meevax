#ifndef INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  class table
  {
    std::vector<std::string> to_string;

  public:
    table(const std::string& s)
    {
      boost::algorithm::split(to_string, s, boost::is_any_of(","));

      for (auto&& each : to_string)
      {
        boost::algorithm::trim(each);
      }
    }

    decltype(auto) operator[](const std::size_t size) const
    {
      return to_string[size];
    }
  };

  // 列挙体の文字列化定義を簡略化するためのクソコード
  #define DEFINE_INSTRUCTION(...)                                              \
                                                                               \
  enum class code                                                              \
  {                                                                            \
    __VA_ARGS__                                                                \
  };                                                                           \
                                                                               \
  struct instruction                                                           \
  {                                                                            \
    const code value;                                                          \
                                                                               \
    template <typename... Ts>                                                  \
    instruction(Ts&&... args)                                                  \
      : value {std::forward<Ts>(args)...}                                      \
    {}                                                                         \
                                                                               \
    static decltype(auto) to_string(const code value)                          \
    {                                                                          \
      static const table transform (#__VA_ARGS__);                             \
                                                                               \
      return transform[                                                        \
        static_cast<typename std::underlying_type<code>::type>(value)          \
      ];                                                                       \
    }                                                                          \
                                                                               \
    decltype(auto) to_string() const noexcept                                  \
    {                                                                          \
      return to_string(value);                                                 \
    }                                                                          \
  };

  DEFINE_INSTRUCTION(
    APPLY, // XXX 紛らわしいから CALL と TAIL_CALL に変える？
    APPLY_TAIL,
    DEFINE,
    JOIN,
    LOAD_GLOBAL,
    LOAD_LITERAL,
    LOAD_LOCAL,
    LOAD_LOCAL_VARIADIC,
    MAKE_CLOSURE,
    MAKE_CONTINUATION,
    MAKE_MODULE,
    POP,
    PUSH,
    RETURN,
    SELECT,
    SELECT_TAIL,
    SET_GLOBAL,
    SET_LOCAL,
    SET_LOCAL_VARIADIC,
    STOP
  )

  std::ostream& operator<<(std::ostream& os, const instruction& instruction)
  {
    return os << "\x1b[32m" << instruction.to_string() << "\x1b[0m";
  }

  static const auto _apply_               {make<instruction>(code::APPLY)};
  static const auto _apply_tail_          {make<instruction>(code::APPLY_TAIL)};
  static const auto _define_              {make<instruction>(code::DEFINE)};
  static const auto _join_                {make<instruction>(code::JOIN)};
  static const auto _load_global_         {make<instruction>(code::LOAD_GLOBAL)};
  static const auto _load_literal_        {make<instruction>(code::LOAD_LITERAL)};
  static const auto _load_local_          {make<instruction>(code::LOAD_LOCAL)};
  static const auto _load_local_variadic_ {make<instruction>(code::LOAD_LOCAL_VARIADIC)};
  static const auto _make_closure_        {make<instruction>(code::MAKE_CLOSURE)};
  static const auto _make_continuation_   {make<instruction>(code::MAKE_CONTINUATION)};
  static const auto _make_module_         {make<instruction>(code::MAKE_MODULE)};
  static const auto _pop_                 {make<instruction>(code::POP)};
  static const auto _push_                {make<instruction>(code::PUSH)};
  static const auto _return_              {make<instruction>(code::RETURN)};
  static const auto _select_              {make<instruction>(code::SELECT)};
  static const auto _select_tail_         {make<instruction>(code::SELECT_TAIL)};
  static const auto _set_global_          {make<instruction>(code::SET_GLOBAL)};
  static const auto _set_local_           {make<instruction>(code::SET_LOCAL)};
  static const auto _set_local_variadic_  {make<instruction>(code::SET_LOCAL_VARIADIC)};
  static const auto _stop_                {make<instruction>(code::STOP)};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

